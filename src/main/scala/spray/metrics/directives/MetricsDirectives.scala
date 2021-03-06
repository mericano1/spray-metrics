/*
 * Copyright (C) 2011-2013 spray.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.metrics
package directives

import akka.actor.Status.Failure
import com.codahale.metrics.{MetricRegistry, Timer}
import spray.http.HttpResponse
import spray.routing.directives.BasicDirectives
import spray.routing.{Directive0, Rejected, RequestContext}

import scala.util.control.NonFatal


/**
 * Provides helper methods for metrics.
 */
object MetricHelpers {
  /**
   * Given a [[spray.routing.RequestContext]], will construct a valid string for
   * the metric name using the HTTP method and the HTTP path.  e.g.
   * `com.mycompany.mynoun.GET`.
   *
   * @param ctx The [[spray.routing.RequestContext]] for which the name should
   * be generated.
   *
   * @return The generated name for the metric.
   */
  def metricName(ctx: RequestContext): String = {
    val methodName = ctx.request.method.name
    val routeName = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
    routeName + "." + methodName
  }

  def around(before: RequestContext ⇒ (RequestContext, Any ⇒ Any)): Directive0 =
    BasicDirectives.mapInnerRoute { inner ⇒
      ctx ⇒
        val (ctxForInnerRoute, after) = before(ctx)
        try inner(ctxForInnerRoute.withRouteResponseMapped(after))
        catch { case NonFatal(ex) ⇒ after(Failure(ex)) }
    }
}

/**
 * The CounterMetric object holds helpers for code that implements Counters.
 */
object CounterMetric {
  // Declares the function type that increments a given metric
  type CountIncrementer = (String, MetricRegistry) ⇒ Unit

  // Increments nothing at all
  val nilIncrementer: CountIncrementer = { (_, _) ⇒ () }

  /**
   * Increments the counter specified by "prefix.postfix" in the given
   * MetricRegistry.
   *
   * @param postfix
   *   The identifier for the counter is defined with some prefix and some
   *   postfix.  The postfix is something like 'successes' or 'failures'.  The
   *   prefix may be much more dynamically determined.
   * @param prefix
   *   The prefix of the counter metric identifier.  This may be a constant
   *   defined elsewhwere or dynamically computed by the route path.
   * @param metricRegistry
   *   The instance of the MetricRegistry in which the counter resides.
   */
  def inc(postfix: String)(prefix: String, metricRegistry: MetricRegistry): Unit =
    metricRegistry.counter(prefix + "." + postfix).inc()

  // An instance of the [[inc]] function for successes
  val incSuccesses = inc("successes") _

  // An instance of the [[inc]] function for failures
  val incFailures = inc("failures") _

  // An instance of the [[inc]] function for rejections
  val incRejections = inc("rejections") _

  // An instance of the [[inc]] function for exceptions
  val incExceptions = inc("exceptions") _
}

/**
 * The CounterBase trait provides helpers for derivations that implement
 * specific types of Counter metrics.
 */
sealed trait CounterBase {
  import CounterMetric.CountIncrementer

  // The instance of the MetricRegistry that holdes this counter metric
  val metricRegistry: MetricRegistry

  // The incrementer for the counter that counts failing metrics
  val handleFailures: CountIncrementer

  // The incrementer for the counter that counts rejecting metrics
  val handleRejections: CountIncrementer

  // The incrementer for the counter that counts excepting metrics
  val handleExceptions: CountIncrementer

  // The incrementer for the counter that counts successful metrics
  val handleSuccesses: CountIncrementer


    /**
   * The [[spray.routing.BasicDirectives#around]] directive requires that the
   * caller return a function that will process what happens <i>after</i> the
   * specific [[spray.routing.Route]] completes.  This method builds that
   * function.
   *
   * @param key
   *   The prefix that we can send to any of the incrementation handlers.
   *
   * @return
   *   The function that can deal with the result of the
   *   [[spray.routing.Route]]'s evaluation.
   */
  def buildAfter(key: String): Any ⇒ Any = { possibleRsp: Any ⇒
    possibleRsp match {
      case rsp: HttpResponse ⇒
        if (rsp.status.isFailure) handleFailures(key, metricRegistry)
        else handleSuccesses(key, metricRegistry)
      case Rejected(_) ⇒
        handleRejections(key, metricRegistry)
      case Failure(_) ⇒
        handleExceptions(key, metricRegistry)
    }
    possibleRsp
  }
}

/**
 * Provides a builder that can provide a new [[spray.routing.Directive]], which
 * will count successful, failed, rejected or excepted operations in a given
 * [[spray.routing.Route]].
 *
 * The actual identifiers for this counter will be, given a specific
 * `prefix`:
 *
 * - {prefix}.successes
 * - {prefix}.failures
 * - {prefix}.rejections
 * - {prefix}.exceptions
 *
 * @constructor Create the counter metric with a specific prefix.
 * @param prefix
 *   The user-specific designation for this counter's Id.
 * @param metricRegistry
 *   The instance of the MetricRegistry that holds the counter metric.
 * @param handleFailures
 *   A function that will increment `{prefix}.failures`. Defaults to
 *   [[spray.metrics.directives.CounterMetric.CountIncrementer.nilIncrementer]].
 * @param handleRejections
 *   A function that will increment `{prefix}.rejections`. Defaults to
 *   [[spray.metrics.directives.CounterMetric.CountIncrementer.nilIncrementer]].
 * @param handleExceptions
 *   A function that will increment `{prefix}.exceptions`. Defaults to
 *   [[spray.metrics.directives.CounterMetric.CountIncrementer.nilIncrementer]].
 * @param handleSuccesses
 *   A function that will increment `{prefix}.successes`. Defaults to
 *   [[spray.metrics.directives.CounterMetric.CountIncrementer.incSuccesses]].
 */
case class CounterMetric(
    prefix: String,
    metricRegistry: MetricRegistry,
    handleFailures: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleRejections: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleExceptions: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleSuccesses: CounterMetric.CountIncrementer = CounterMetric.incSuccesses) extends CounterBase {

  import CounterMetric._
  import MetricHelpers._

  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val count: Directive0 = around { ctx ⇒ (ctx, buildAfter(prefix)) }

  /**
   * Returns a new instance of the CounterMetric that will <i>not</i> count
   * successes. Any other counting aspect will remain as it was.
   */
  def noSuccesses: CounterMetric = copy(handleSuccesses = nilIncrementer)

  /**
   * Returns a new instance of the CounterMetric that will count rejections. Any
   * other counting aspect will remain as it was.
   */
  def rejections: CounterMetric = copy(handleRejections = incRejections)

  /**
   * Returns a new instance of the CounterMetric that will count failures. Any
   * other counting aspect will remain as it was.
   */
  def failures: CounterMetric = copy(handleFailures = incFailures)

  /**
   * Returns a new instance of the CounterMetric that will count exceptions. Any
   * other counting aspect will remain as it was.
   */
  def exceptions: CounterMetric = copy(handleExceptions = incExceptions)

  /**
   * Returns a new instance of the CounterMetric that will count successes,
   * failures, rejections and exceptions.
   */
  def all: CounterMetric = copy(handleFailures = incFailures,
    handleRejections = incRejections,
    handleExceptions = incExceptions,
    handleSuccesses = incSuccesses)
}

/**
 * Provides a builder that can provide a new [[spray.routing.Directive]], which
 * will count successful, failed, rejected or excepted operations in a given
 * [[spray.routing.Route]].
 *
 * The actual identifiers for this counter will be, depending on the incoming
 * URL (e.g. `/path/to/route` translates to `path.to.route`):
 *
 * - {path.to.route}.successes
 * - {path.to.route}.failures
 * - {path.to.route}.rejections
 * - {path.to.route}.exceptions
 *
 * @constructor Create the counter metric that will use the route path for the
 * metric name.
 * @param metricRegistry
 *   The instance of the MetricRegistry that holds the counter metric.
 * @param handleFailures
 *   A function that will increment `{prefix}.failures`. Defaults to
 *   [[CountIncrementer.nilIncrementer]].
 * @param handleRejections
 *   A function that will increment `{prefix}.rejections`. Defaults to
 *   [[CountIncrementer.nilIncrementer]].
 * @param handleExceptions
 *   A function that will increment `{prefix}.exceptions`. Defaults to
 *   [[CountIncrementer.nilIncrementer]].
 * @param handleSuccesses
 *   A function that will increment `{prefix}.successes`. Defaults to
 *   [[CountIncrementer.incSuccesses]].
 */
case class CounterMetricByUri(
    metricRegistry: MetricRegistry,
    handleFailures: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleRejections: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleExceptions: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleSuccesses: CounterMetric.CountIncrementer = CounterMetric.incSuccesses) extends CounterBase {

  import CounterMetric._
  import MetricHelpers._

  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val count: Directive0 = around { ctx ⇒
    (ctx, buildAfter(metricName(ctx)))
  }

  /**
   * Returns a new instance of the CounterMetric that will <i>not</i> count
   * successes. Any other counting aspect will remain as it was.
   */
  def noSuccesses: CounterMetricByUri = copy(handleSuccesses = nilIncrementer)

  /**
   * Returns a new instance of the CounterMetric that will count rejections. Any
   * other counting aspect will remain as it was.
   */
  def failures: CounterMetricByUri = copy(handleFailures = incFailures)

  /**
   * Returns a new instance of the CounterMetric that will count failures. Any
   * other counting aspect will remain as it was.
   */
  def rejections: CounterMetricByUri = copy(handleRejections = incRejections)

  /**
   * Returns a new instance of the CounterMetric that will count exceptions. Any
   * other counting aspect will remain as it was.
   */
  def exceptions: CounterMetricByUri = copy(handleExceptions = incExceptions)

  /**
   * Returns a new instance of the CounterMetric that will count successes,
   * failures, rejections and exceptions.
   */
  def all: CounterMetricByUri = copy(handleFailures = incFailures,
    handleRejections = incRejections,
    handleExceptions = incExceptions,
    handleSuccesses = incSuccesses)
}

/**
 * The TimerBase trait provides helpers for derivations that implement specific
 * types of Timer metrics.
 */
sealed trait TimerBase {
  /**
   * The [[spray.routing.BasicDirectives#around]] directive requires that the
   * caller return a function that will process what happens <i>after</i> the
   * specific [[spray.routing.Route]] completes.  This method builds that
   * function.
   *
   * @param timerContext
   *   The context of the Timer that was originally started in the `before` part
   *   of the [[spray.routing.BasicDirectives#around]]
   *   [[spray.routing.Directive]].
   *
   * @return
   *   The function that will stop the timer on any result whatsoever.
   */
  def buildAfter(timerContext: Timer.Context): Any ⇒ Any = { possibleRsp: Any ⇒
    possibleRsp match {
      case _ ⇒
        timerContext.stop()
    }
    possibleRsp
  }
}

/**
 * Provides a Timer metric that will record times on the timer under the name
 * given.
 *
 * @constructor Create the timer metric with a specific name.
 * @param timerName
 *   The name for this particular timer.
 * @param metricRegistry
 *   The instance of the MetricRegistry in which the timer metric exists.
 */
case class TimerMetric(timerName: String, metricRegistry: MetricRegistry) extends TimerBase {
  import MetricHelpers._
  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val time: Directive0 =
    around { ctx ⇒
      val timerContext = metricRegistry.timer(timerName).time()
      (ctx, buildAfter(timerContext))
    }
}

/**
 * Provides a Timer metric that will record times based on the current HTTP
 * route.
 *
 * @constructor Create the timer metric that will use the current path route for
 * the metric name.
 * @param metricRegistry
 *   The instance of the MetricRegistry in which the timer metric is to be
 *   created.
 */
case class TimerMetricByUri(metricRegistry: MetricRegistry) extends TimerBase {
  import MetricHelpers._

  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val time: Directive0 =
    around { ctx ⇒
      val timerContext = metricRegistry.timer(metricName(ctx)).time()
      (ctx, buildAfter(timerContext))
    }
}

/**
 * Provides a Meter metric that will mark a specific event under a Meter of a
 * specific name.
 *
 * @constructor Create the meter metric with a specific name.
 * @param meterName
 *   The name under which the meter exists.
 * @param metricRegistry
 *   The instance of the MetricRegistry in which the meter metric exists.
 */
case class MeterMetric(meterName: String, metricRegistry: MetricRegistry) {
  import spray.routing.directives.BasicDirectives._

  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val meter: Directive0 = mapRequestContext { ctx ⇒
    metricRegistry.meter(meterName).mark()
    ctx
  }
}

/**
 * Provides a Meter metric that will mark a specific event under a Meter that
 * has an identifier that matches the current [[spray.routing.Route]] path.
 *
 * @constructor Create the meter metric that will use the current path route for
 * the metric name.
 * @param metricRegistry
 *   The instance of the MetricRegistry in which to create the meter metric.
 */
case class MeterMetricByUri(metricRegistry: MetricRegistry) {
  import MetricHelpers._
  import spray.routing.directives.BasicDirectives._

  /**
   * This is the instance of the [[spray.routing.Directive]] that you can use in
   * your [[spray.routing.Route]].
   */
  val meter: Directive0 = mapRequestContext { ctx ⇒
    metricRegistry.meter(metricName(ctx)).mark()
    ctx
  }
}

/**
 * Provides an entry point to creating metric accumulators specific to the Coda
 * Hale Metrics library (http://metrics.codahale.com).
 *
 * ==Overview==
 *
 * This trait is intended to be used to construct objects that provide
 * [[spray.routing.Directive]]s, which can then be used to instrument your
 * [[spray.routing.Route]]s with metrics accumulators.  You would create these
 * instances, and then join them together however you like in order to ease how
 * your code is instrumented.
 *
 * ==Usage==
 *
 * {{{
 * import com.codahale.metrics.MetricRegistry
 *
 * class MyApp extends Directives {
 *   val metricRegistry = new MetricRegistry()
 *   val metricFactory = CodaHaleMetricsDirectiveFactory(metricRegistry)
 *
 *   // Creates a counter that measures failures only under the name of the
 *   // path to the current route
 *   val counter = metricFactory.counter.failures.noSuccesses.count
 *
 *   // Creates a timer that measures everything under the name of the
 *   // path to the current route
 *   val timer = metricFactory.timer.time
 *
 *   // Joins the two metrics into a single directive
 *   val measure = counter | timer
 *
 *   val apiRoute =
 *     path("something") {
 *       measure {
 *         get {
 *           // do the thing
 *         }
 *       }
 *     }
 * }
 * }}}
 */
trait CodaHaleMetricsDirectiveFactory {
  // The instance of the MetricRegistry in which you want to store your metrics
  val metricRegistry: MetricRegistry

  /**
   * Creates an instance of a [[CounterMetric]] with a specific prefix name that
   * counts successes by default.
   *
   * @param counterPrefix
   *   The prefix of the counter's identifier.
   *
   * @return
   *   The instance of the [[CounterMetric]] you can use to count events.
   */
  def counter(counterPrefix: String): CounterMetric = new CounterMetric(counterPrefix, metricRegistry)

  /**
   * Creates an instance of a [[CounterMetric]] with a specific prefix name that
   * counts all types of events.
   *
   * @param counterPrefix
   *   The prefix of the counter's identifier.
   *
   * @return
   *   The instance of the [[CounterMetric]] you can use to count events.
   */
  def allCounter(counterPrefix: String): CounterMetric = new CounterMetric(counterPrefix, metricRegistry).all

  /**
   * Creates an instance of a [[CounterMetric]] that counts successes by
   * default under an identifier unique to the path to the current route..
   *
   * @return
   *   The instance of the [[CounterMetric]] you can use to count events.
   */
  def counter: CounterMetricByUri = new CounterMetricByUri(metricRegistry)

  /**
   * Creates an instance of a [[CounterMetric]] that counts all types of events
   * under an identifier unique to the path to the current route..
   *
   * @return
   *   The instance of the [[CounterMetric]] you can use to count events.
   */
  def allCounter: CounterMetricByUri = new CounterMetricByUri(metricRegistry).all

  /**
   * Creates an instance of a [[TimerMetric]] that measures events with a
   * specific name.
   *
   * @param timerName
   *   The name of the timer in which measured events should be recorded.
   *
   * @return
   *   The instance of the [[TimerMetric]] you can use to measure events.
   */
  def timer(timerName: String): TimerMetric = new TimerMetric(timerName, metricRegistry)

  /**
   * Creates an instance of a [[TimerMetric]] that measures events with a
   * name specific to the path to the current route.
   *
   * @return
   *   The instance of the [[TimerMetric]] you can use to measure events.
   */
  def timer: TimerMetricByUri = new TimerMetricByUri(metricRegistry)

  /**
   * Creates an instance of a [[MeterMetric]] that measures events with a
   * specific name.
   *
   * @param meterName
   *   The name of the meter in which measured events should be recorded.
   *
   * @return
   *   The instance of the [[MeterMetric]] you can use to measure events.
   */
  def meter(meterName: String): MeterMetric = new MeterMetric(meterName, metricRegistry)

  /**
   * Creates an instance of a [[MeterMetric]] that measures events with a
   * name specific to the path to the current route.
   *
   * @return
   *   The instance of the [[MeterMetric]] you can use to measure events.
   */
  def meter: MeterMetricByUri = new MeterMetricByUri(metricRegistry)
}

/**
 * Provides construction for an instance of the CodaHaleMetricsDirectiveFactory.
 */
object CodaHaleMetricsDirectiveFactory {
  /**
   * Constructs and instance of the CodaHaleMetricsDirectiveFactory around a
   * specific instance of a MetricRegistry.
   *
   * @param registry
   *   The instance of the MetricRegistry that this factory should use.
   * @return
   *   The instance of the CodaHaleMetricsDirectiveFactory to be used.
   */
  def apply(registry: MetricRegistry) = new CodaHaleMetricsDirectiveFactory {
    val metricRegistry = registry
  }
}
