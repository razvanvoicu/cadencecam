CadenceCam
==========

CadenceCam counts exercise repetitions with a phone's camera. A phone propped where it can see a repetitive
movement — a dumbbell curl, a stair climber's rotating wheel, a stationary bicycle's pedal — counts the reps as they
happen, and a tablet or a second phone can show the running count, the pace, and an estimate of the energy used.

The picture never leaves the phone. The browser reads only how bright each quarter of the frame is, ten times a
second, and counts the movement from that; no photo or video is recorded or uploaded.

It is a Scala 3 web application: a Scala.js frontend served by a ZIO HTTP backend from one deployment artifact, with
Firestore for storage. The backend's infrastructure — Google login, browser sessions, discovered and
capability-checked plugins — began as a reusable web-application template, and the later sections of this document
describe it in those terms.

Using CadenceCam
----------------

CadenceCam is deployed at https://cadencecam.raz.sg and is free to use by anyone with a Google account. Of that
account it uses only your name and email address. Read the `privacy policy <https://cadencecam.raz.sg/privacy.html>`_
and the `terms of service <https://cadencecam.raz.sg/tos.html>`_; both are also in the ☰ menu on every screen, where
they open in a tab of their own.

You need a phone for the camera and, if you want a separate display, a tablet or a second phone signed in to the same
Google account. Both run in the browser; either can also be installed to the home screen from the browser's menu.

1. **Count.** Sign in on the phone. If nothing is counting for your account yet, it goes straight to the **Counter**
   screen and asks for the camera. Prop it where the moving part of the exercise fills a good share of the picture,
   and start. For the first fifteen seconds or so the counter works out the cadence; then the count appears, including
   the reps done while it was working it out. The badge beside the status line says how far the movement stands out
   from the background: green counts reliably, amber is marginal, red is likely to miss reps. ↺ starts the count over,
   which is how to discard the movement of getting into position.
2. **Watch.** Sign in on the tablet and choose **Dashboard**. It connects directly to the counting phone — the two
   need to be on the same network — and shows reps and calories, each with the pace over the last ten reps and a
   projection to the next half-hour mark. Its ↺ resets the counter from across the room.
3. **Settings** (dashboard or counter menu): your weight, and the exercises you do, each with a factor and a choice of
   whether energy is worked out from the rep count or from the rep frequency, where faster reps count for more.
   Calories are ``measure × weight in kg × factor / 100``; the measure is the number of reps, or for frequency the sum
   of each rep's rate in reps per second. The factor is yours to calibrate against your equipment, and starts at 1.25.
4. **History** (dashboard menu): every workout, newest first, with when it started, how long it ran, its reps and its
   calories. Any workout can be deleted, and the history's own menu exports the list as a CSV file.
5. **Default to dashboard on / off** (dashboard menu): makes this device open straight on the dashboard at every
   start. It is remembered by this browser only, so the phone signed in to the same account still opens its camera.
6. **Delete all my data** (dashboard menu): removes every workout, recording and setting kept for the account, then
   signs you out.

On the counter, the menu switches between cameras, and **Camera controls** set exposure and sensitivity by hand when
automatic exposure gets the picture wrong (**Back to automatic** undoes that). **Capture signal trace** uploads the
last few minutes of the four brightness signals, for working on the detector. Only one device counts for an account at
a time: choosing **Take over counting** on another device sends the current counter back to its home screen. A workout
ends when you log out, or ten minutes after the counter last reported — when its page is closed or its phone sleeps.

How it works
------------

**The counter** runs entirely in the phone's browser (``sgrv.fe.acquire``). ``FrameSampler`` draws the camera
preview into a tiny off-screen canvas ten times a second and takes the mean luma of each quadrant of the frame,
producing four brightness signals that ``QuadrantSignals`` keeps for six minutes. On every sample ``RepCounter``
re-analyses the last fifteen seconds of each quadrant: a 0.5–2 Hz band-pass filter (``Biquad``), peak detection by
prominence and minimum spacing (``PeakDetector``), and rejection of peaks that do not stand up against the reps
before them or never return (``RepAnalysis``). Nothing is counted until two quadrants agree on the cadence. Each
agreeing quadrant then keeps its own tally from its own peaks, and the count shown is the largest tally another
quadrant corroborates to within a couple of reps; it never goes down. ``RepCountStore`` keeps the count in
``localStorage``, so a reload mid-set resumes it. ``Camera`` opens the camera's widest mode and, once metering has
settled, holds its exposure, white balance and focus still, then checks the picture against the one automatic had
produced and lets go if holding changed it. The detector's tuning is gathered in ``DetectorSettings``; the values
settled by replaying recorded traces say so in their comments.

**Watching** keeps the server out of the path. A watching device and the counter open a WebRTC data channel between
them (``sgrv.fe.live.PeerLink``). The offer, the answer and the ICE candidates are exchanged through Firestore by the
``/live/signal`` routes, and no STUN or TURN server is configured, which is why the two must share a network. The
counter sends a ``LiveReading`` whenever what it would say changes — the count, its status line, the elapsed time,
the accumulated cadence — and the dashboard works pace, calories and projections out of those (``sgrv.fe.Effort``).
The dashboard's two commands, reset and capture a trace, travel back on the same channel as ``LiveCommand``.

**The account's session** is the record a workout leaves behind (``sgrv.be.sessions``). Taking the counter's role
(``POST /live/acquirer``) opens a session in Firestore, or moves the one in progress to this device. The counter then
reports its count, cadence and elapsed time every ten seconds (``POST /countingSession/reps``). A report from a device
that is no longer the counter is refused with ``409``, which is how a displaced device learns to stand down. Logging
out closes the session, and so does ten minutes without a report; closed sessions are the history. See
`Data model`_.

**The bench** (``sgrv.fe.bench``, offered only with ``#test`` at the end of the address) is a development
instrument. A desktop screen animates a figure at a known cadence for a phone on a tripod to count, compares the
count with the truth as it goes, files what it observes as test events (``POST /test/event``), and has the counter
capture a signal trace after each test (``POST /countingSession/trace``). Abandoning a run deletes its events and
traces (``POST /test/run/discard``).

Every shape the two ends exchange is defined once, in the ``shared`` project (``sgrv.api``), together with the route
it travels on where it has one, so the frontend and the backend cannot drift apart. The ☰ menu on every screen carries
**About**, the two documents and — once signed in — **Logout**. About shows the signed-in account, the build
information from the authenticated ``GET /about`` route, and the frontend build the browser is actually running;
before sign-in it shows only the last. Logout invokes ``POST /logout`` and returns the browser to the signed-out home
page only after the server has revoked the Google grant and removed the browser session. The login screen explains
what the app does before asking anyone to sign in.

The backend owns the static-file routes, but application API routes are not
coupled to ``Main``. ``RouteDiscovery`` scans the ``sgrv.be`` package on the
runtime classpath for objects implementing the nominal ``BackendPlugin`` interface. Each plugin returns native
ZIO HTTP ``Routes`` and couples their environment type to a runtime ``CapabilitySet``. The loader resolves that
set from the services supplied by ``Main``, closes the routes over precisely that environment, and activates the
plugin. A plugin with missing capabilities, an incompatible API version, an initialization failure, or a route
conflict is reported and isolated without preventing other plugins from loading.

Capabilities are host facilities, not plugin implementations. The host exposes generic services such as the
HTTP client and session store; a plugin owns whatever API-specific adapter it needs and constructs it from
those generic facilities. Consequently, adding a plugin does not require adding its private services to
``Main`` or ``BackendEnvironment``.

The Scala.js linker runs as a backend resource generator. Its ``main.js`` and
source map are copied into the backend's managed ``web`` resources beside the
hand-written HTML, CSS, web-app manifest, favicon, PWA icons, and the two documents. Consequently, one backend
build contains and serves the complete application. A second resource generator captures the version, build
timestamp, build OS, Scala version, and Scala.js plugin version in a packaged ``build-info.properties`` resource
for ``/about``.

Technology
----------

* Scala 3.8.4
* Scala.js and Laminar for the browser application, with zio-json for the shapes shared with the backend
* WebRTC data channels between a counting device and the devices watching it
* ZIO HTTP for the backend server, and ZIO Logging for console logging
* Google OAuth 2.0 (google-api-client) for "Login with Google", requesting only ``openid email profile``
* Google Cloud Firestore for browser sessions, the accounts' workouts and settings, and pairing messages
* ClassGraph for discovering independently loadable, capability-checked backend plugins
* MUnit for frontend and backend tests, and sbt-scoverage for backend coverage
* Selenium (in a separate ``e2etest`` project) for end-to-end browser tests against the real, running app

Running locally
---------------

The build requires **JDK 21 or newer**, Scala 3.8.4, and sbt 1.12.14. JDK 21 is a floor, not a pin: a newer JDK
is a perfectly good machine to develop on, and different clones need not agree on one. Two settings keep that
from mattering to the output, both driven by ``minimumJdkVersion`` in ``build.sbt``:

* ``ThisBuild / initialize`` refuses to load the build on anything older than that version, naming the JDK it
  found and where it lives, rather than failing later in a way that has to be diagnosed.
* ``-release`` (and the matching ``--release`` for javac) compiles against exactly that JDK's API and emits its
  bytecode, whichever JDK is actually running. A call to a newer JDK's API is then a compile error on every
  machine, instead of compiling cleanly on the one machine that has it and failing at runtime everywhere else —
  including inside the ``eclipse-temurin:21-jre-jammy`` runtime image. Keep ``minimumJdkVersion`` and that base
  image in step if you raise either.

Before running the application, create the shared build configuration described
below. From the repository root, run:

.. code-block:: console

   sbt run

Then open the configured ``LOCAL_BASE_URL``.

The server binds to IPv4 loopback (``127.0.0.1``). ``sbt run`` derives ``PORT``
from the explicit port in ``LOCAL_BASE_URL``. The backend has no port default and
does not accept a command-line port: a missing, non-numeric, or out-of-range
``PORT`` stops startup with a clear configuration error.

Static-file expiry has no compiled-in default. Both ``test.env`` (the local ``run`` task) and ``prod.env`` (the
standalone ``artifact`` image and ``deployGCloud``) currently set ``STATIC_ASSET_CACHE_MAX_AGE_SECONDS=0``,
because every deployment is still a test deployment and a cached ``style.css`` or ``main.js`` outlives a server
restart, hiding a change that was in fact shipped. Raise ``prod.env`` to a real expiry (``86400`` is one day)
once deployments become genuinely production. A missing, non-numeric, or negative value stops startup with a
configuration error. ``index.html`` remains ``no-cache`` so a launch can discover a new application build.

On Windows, the development server can be stopped by port with:

.. code-block:: powershell

   ./scripts/stopapp.ps1 -Port <local-port>

``sbt run`` sets ``-Djava.net.preferIPv4Stack=true`` on the forked JVM (``run / javaOptions`` in ``build.sbt``),
and the Docker image's ``runApp`` launcher passes the same flag directly. Some networks hand out an
IPv6 (AAAA) address for ``googleapis.com`` without actually routing IPv6, which otherwise surfaces as
``io.netty.channel.AbstractChannel$AnnotatedNoRouteToHostException`` /
``java.net.NoRouteToHostException`` from outbound Google API calls (the OAuth token exchange, and the token
refresh behind session renewal); forcing IPv4 avoids that entirely.

Routes and caching
------------------

Static files are wired directly in ``Main``, and reserved against conflicting routes from discovered plugins.
``index.html`` is always ``no-cache``, so a launch discovers a new build. Everything else static is cached for
``STATIC_ASSET_CACHE_MAX_AGE_SECONDS``, which both ``test.env`` and ``prod.env`` currently set to ``0`` (see
`Running locally`_).

.. list-table::
   :header-rows: 1
   :widths: 34 66

   * - Static route
     - Content
   * - ``/``, ``/index.html``
     - The page that loads the application
   * - ``/main.js``, ``/main.js.map``
     - The linked Scala.js application and its source map
   * - ``/style.css``
     - Hand-written stylesheet
   * - ``/manifest.webmanifest``
     - PWA identity, launch behavior, colors, and icons
   * - ``/favicon.ico``, ``/icon-192.png``, ``/icon-512.png``
     - The application icon, for browser tabs and home screens
   * - ``/icon-maskable-192.png``, ``/icon-maskable-512.png``
     - The same icon inside the safe zone Android's launcher masks cut to
   * - ``/privacy.html``, ``/tos.html``
     - The privacy policy and the terms of service: self-contained pages that need neither the application nor a
       session

Every other route comes from a discovered plugin and answers with ``Cache-Control: no-store``, except the two OAuth
redirects.

.. list-table::
   :header-rows: 1
   :widths: 34 16 50

   * - Route
     - Access
     - Purpose
   * - ``GET /auth/login``
     - Public
     - Redirects to Google's login page
   * - ``GET /auth/callback``
     - Public
     - Completes the Google login, then redirects to ``/``
   * - ``GET /me``
     - Public
     - The signed-in user as JSON, or ``401``
   * - ``POST /refreshSession``
     - Public
     - Validates the stored Google refresh token; renews Firestore/cookie expiry and reports it in a header
   * - ``POST /logout``
     - Signed in
     - Revokes Google authorization, deletes the browser session, closes the account's counting session, and expires
       the authentication cookies
   * - ``GET /about``
     - Signed in
     - Build metadata as JSON
   * - ``GET /live/acquirer``
     - Signed in
     - Whether the account already has a device counting
   * - ``POST /live/acquirer``
     - Signed in
     - Takes the counter's role for this browser, opening the account's session when it has none
   * - ``POST /live/signal``, ``GET /live/signal``
     - Signed in
     - Files one step of a WebRTC exchange; hands over what the other end has filed since a cursor
   * - ``POST /countingSession/reps``
     - Signed in
     - The counter's progress report; ``409`` when this browser is no longer the counter
   * - ``POST /countingSession/trace``
     - Signed in
     - Files a captured signal recording under the workout in progress
   * - ``GET /countingSession/history``
     - Signed in
     - The account's workouts, newest first
   * - ``POST /countingSession/history/discard``
     - Signed in
     - Deletes one workout, with its recordings and pairing messages
   * - ``GET /account/settings``, ``PUT /account/settings``
     - Signed in
     - Reads and replaces the account's weight and exercises
   * - ``DELETE /account/data``
     - Signed in
     - Deletes everything kept for the account
   * - ``POST /test/event``
     - Signed in
     - Files one observation from a bench run
   * - ``POST /test/run/discard``
     - Signed in
     - Deletes an abandoned bench run's events and recordings
   * - ``GET /debug``
     - Signed in, and ``?pwd=``
     - Conditional Debug plugin: the backend's system signature

Each plugin declares an ``AccessPolicy``; see `Adding a backend plugin`_. ``/auth/login``, ``/auth/callback``, and
``/me`` use ``AccessPolicy.Public`` because they must serve visitors without an existing session. When linked,
the Debug plugin uses ``AuthenticatedAndAdminPassword``, so reaching ``/debug`` needs both a session and the
admin password (`Admin-protected routes`_). Session refresh is public so it can recover an expired session; it
requires the opaque cookie and a still-valid stored Google refresh token before extending Firestore and reissuing
the ``HttpOnly`` cookie. Everything else uses ``Authenticated``. Logout consumes the resulting authenticated request
context to reach the signed-in user's stored Google refresh token, revoking their Google credentials and
invalidating the current session without a second Firestore lookup. Every route that reads or writes an account's
data takes the account from the session, never from the request, so no request can name somebody else's.

The Scala.js frontend continues to use ``/me`` as its read-only startup check. A successful result enables the
Scala-written ``SessionRefreshWorker``, which renews immediately and reads ``X-Session-Expires-At`` from the
response. It schedules its next renewal five minutes before that expiry; the lead time is a constructor setting,
while transient non-``401`` failures retry after one minute. Logout disables the worker before contacting the
backend.

Application UI state is an immutable, typed ``FrontendState`` snapshot encoded with ``zio-json`` under the browser
``localStorage`` key ``sgrv.frontend-state.v3``. A locally scoped ``FrontendStateStore`` given owns both persistence
and Laminar's reactive projection, so DOM updates remain declarative and the two representations cannot be updated
separately. On page load, transient operations are normalized and authentication returns to ``Unknown`` until
``/me`` confirms the HttpOnly cookie against the backend; persisted state is never treated as proof of
authentication.

The browser keeps three other things of its own, each under its own key and each read defensively, so storage
that is missing or full costs a convenience rather than the app: the running count, with its cadence and clock,
under ``sgrv.rep-count.v2`` (``RepCountStore``; resumed only within an hour of its last change); the device's own
start-up preference under ``default_to_dashboard``, with the one-shot ``sgrv.present-roles-on-next-start`` marker
that makes turning it off take effect at the next start (``StartPreference``); and the session-renewal state below.

Session-renewal infrastructure lives separately in ``sgrv.fe.refreshstate`` and persists only ``RefreshState``
under ``sgrv.refresh-state.v1``. That package has no dependency on any application-specific state, so the
application above it can be replaced without modifying session refresh. Browser-only resources such as
timeout handles are not serialized. Instead, scheduled refresh callbacks validate a persisted generation before
acting, so disabling or replacing a worker makes older callbacks harmless.

Every frontend request passes through the stateless ``HttpService``. It reports every ``401`` to the session
lifecycle owner; that owner reacts only while persisted session monitoring is active. Consequently the expected
initial ``/me`` response can still represent an ordinary signed-out visitor, while the first relevant ``401`` from
the renewal worker or any other HTTP call disables the worker, clears authenticated UI state, and opens a
session-expired modal. Requests are not implicitly retried and the modal does not initiate OAuth; reloading returns
to the normal login page. New frontend routes inherit this behavior by using ``HttpService`` rather than calling
``dom.fetch`` directly.

A missing cookie, missing retained Firestore record, missing refresh token, or Google refresh rejection produces
``401``. Firestore failures produce ``503`` instead. Renewal can only recover a record that Firestore TTL has not
yet deleted and for which the browser still supplies the opaque cookie; scheduled renewal normally extends both
before they expire.

PWA installation
----------------

The application is installable from supporting browsers. ``index.html`` declares ``manifest.webmanifest``, the 192×192
and 512×512 PNG icons, theme colors, and an Apple touch icon. The manifest launches the installed application at ``/``
in a standalone window, and lists each icon twice: once as ``any``, and once as ``maskable``, drawn inside the central
safe zone that Android launchers cut their own shape from. Without the maskable pair a Pixel shows the plain icon
shrunk onto a white disk. The icons are drawn as SVG under ``scripts/icon/``, whose README describes how the PNGs and
the favicon are produced from them. Installation does not enable application-shell or offline caching; the app needs a
connection to launch and use backend data.

Installation requires HTTPS in a deployed environment; browsers also accept ``localhost`` and ``127.0.0.1`` for
local development. Use the browser's install action after loading the application. Both image-building tasks
verify that the manifest, favicon, and required PNG icons are present in the packaged backend JAR before
staging their Docker context.

Data model
----------

Everything the backend keeps is in the project's ``(default)`` Firestore database, in three collections.

``Access``
   One document per browser session, expired by a TTL policy; see `Login with Google`_.

``CountingSessions``
   One document per account, named by a keyed digest of the account's email — HMAC-SHA256 under ``ACCOUNT_KEY`` —
   so no email appears in a document path, and nobody holding the database can confirm whether an address has an
   account without also holding the key. ``AccountSchema`` in ``AccountSessions.scala`` is the authority on the
   fields:

   .. code-block:: text

      CountingSessions/{digest}      activeSession, counter, lastRepAt, settings (AccountSettings as JSON)
        sessions/{session id}        startedAt, completedAt, endedBy, counter,
                                     reps, repsAt, cadenceSum, elapsedSeconds
          traces/{trace id}          one captured signal recording (TraceSchema)
          signals/{auto id}          one step of a WebRTC exchange

   ``activeSession`` points at the workout in progress, so "one session per account" is a property of the shape
   rather than of a query. ``counter`` is a SHA-256 of the counting browser's session key: it says which device may
   report, and a device taking the role over changes it without starting a new workout. Every accepted report moves
   ``lastRepAt``; a session whose mark is older than ``COUNTING_IDLE_MINUTES`` (ten by default) is closed as ``Idle``
   by whichever request next looks at it — there is no timer. Closed sessions are kept, because they are the
   history, and a calorie figure is never stored: only what it is worked out from, since the weight and the factor
   behind it can change afterwards.

``TestEvents``
   One document per bench observation, filed with the email of the account that ran the bench.

``ACCOUNT_KEY`` comes from the file named by ``ACCOUNT_KEY_PATH`` in the shared configuration — one line of at
least 32 characters, such as the output of ``openssl rand -hex 32`` — and every launch and deployment task hands it
to the backend. Without it no account can be named: reads come back empty, writes answer ``503``, and no session
opens. There is deliberately no unkeyed fallback, which would give up the property the key exists for.

Login with Google
-----------------

OAuth configuration file
~~~~~~~~~~~~~~~~~~~~~~~~

All machine- and deployment-specific build settings live in one external ``config.env``. The repository only
contains an indirection to it: create one regular file directly under the Git-ignored ``.local/`` directory whose
first line is:

.. code-block:: text

   APPCONFIGPATH=/path/to/shared/config.env

Exactly one file must have such a first line. The path may be absolute or relative to the repository root. Loading
the sbt build fails if the pointer is absent or empty, multiple pointers match, or the referenced file does not
exist. Loading also requires ``OAUTH_CONFIG_PATH`` to resolve to a file. The external file uses strict
``NAME=value`` syntax; blank lines and lines beginning with ``#`` are ignored, and duplicate, malformed, or unknown
settings fail validation.

A complete configuration has this shape:

.. code-block:: text

   OAUTH_CONFIG_PATH=oauth.config.json
   ACCOUNT_KEY_PATH=account.key
   # ADMIN_PASSWORD_PATH=admin.pwd
   LOCAL_BASE_URL=http://localhost:<local-port>
   ARTIFACT_BASE_URL=https://<standalone-host>
   PUBLIC_BASE_URL=https://<cloud-run-host>
   ARTIFACT_PORT=<standalone-container-port>
   GCP_PROJECT_ID=<project-id>
   FIRESTORE_DATABASE_ID=(default)
   GCLOUD_REGION=<cloud-run-region>
   ARTIFACT_REGISTRY_REPOSITORY=<repository-name>
   GCLOUD_SERVICE_ACCOUNT=<runtime-service-account-email>

Relative paths in ``OAUTH_CONFIG_PATH``, ``ACCOUNT_KEY_PATH`` and ``ADMIN_PASSWORD_PATH`` are resolved from the
directory containing ``config.env``. This makes the configuration and its referenced secret files portable as one shared directory,
while every development machine needs only its own small ``APPCONFIGPATH`` locator.

The compulsory OAuth JSON must use Google's standard ``Web application`` structure
and contain ``web.client_id`` and ``web.client_secret``. For ``sbt run``, the build parses these fields into
``GOOGLE_OAUTH_CLIENT_ID`` and ``GOOGLE_OAUTH_CLIENT_SECRET`` and supplies them alongside the values from
``test.env`` without modifying that tracked file. For ``sbt artifact``, it appends the same values only to the
generated ``prod.env`` staged into the Docker build context; the source ``prod.env`` remains secret-free. The
resulting Docker image therefore contains a client secret and must be handled as a secret-bearing artifact (see
`Packaging and deployment`_). ``sbt deployGCloud`` instead supplies the values to the Cloud Run revision and
keeps them out of its image. The optional admin-password path follows the corresponding mechanism and is
described in `Admin-protected routes`_.

The backend reads the OAuth client ID and secret directly from its environment; it does not copy them into
Firestore. The external JSON is the local source of truth. A standalone artifact receives the values from its
generated ``prod.env``; a Cloud Run revision receives them as runtime configuration.

``PUBLIC_BASE_URL`` is the backend's runtime name for the externally visible origin, without a path, query, or
fragment. No origin is committed in either ``test.env`` or ``prod.env``. Instead, each launch/deployment task
reads its setting from the external configuration and injects the selected value into the process as
``PUBLIC_BASE_URL``. The value is required and validated before that task proceeds. Non-local origins must use
HTTPS; plain HTTP is accepted only for ``localhost``, ``127.0.0.1``, and ``::1``. The backend always uses
``PUBLIC_BASE_URL + /auth/callback`` for both sides of the OAuth code exchange and for secure-cookie selection;
request ``Host`` and forwarding headers have no influence on it.

``sbt run`` reads ``LOCAL_BASE_URL``; ``sbt artifact`` reads ``ARTIFACT_BASE_URL``; and ``sbt deployGCloud`` reads
``PUBLIC_BASE_URL``. Each task validates its value and exposes it to the backend as ``PUBLIC_BASE_URL``. The local
URL must include an explicit port, which is also injected as ``PORT``. The standalone artifact receives
``ARTIFACT_PORT``. Cloud Run supplies ``PORT`` itself. The shared file is read at task execution, so edits are
picked up by the next task invocation without an sbt reload.

Authentication uses the server-side OAuth 2.0 authorization-code flow.
``/auth/login`` redirects the browser to Google with a CSRF-protecting
``state`` cookie. Google redirects back to ``/auth/callback``, where the
backend exchanges the authorization code, verifies the Google-signed ID token
(signature, audience, issuer, expiry, and verified email), creates a new
browser-session document in Firestore, and sets an HttpOnly cookie containing
only that session's random key before redirecting to the home page. The
frontend calls ``GET /me`` to resolve the session. The frontend represents the
result explicitly as signed in, unauthenticated (``401``), or authentication
failed (malformed responses, unexpected status codes, and network failures).
OAuth tokens, the client secret, and Firestore writes never reach frontend
JavaScript. The HttpOnly attribute also prevents JavaScript from reading the
session key, although same-origin JavaScript can still issue requests carrying
the cookie.

After sign-in, the frontend's Logout control sends an authenticated ``POST /logout``. The backend first posts the
stored refresh token to Google's OAuth revocation endpoint; if Google issued no refresh token at login, the access
token retained solely for this fallback is revoked instead. An already expired or revoked token is treated as an
idempotent success. It then deletes the current ``Access`` document and returns ``204 No Content`` with expired
``session`` and ``auth_state`` cookies. The frontend reloads ``/``, where ``GET /me`` produces ``401`` and the login
link is shown again. Google revocation is intentionally performed before Firestore deletion: if Google or Firestore
is temporarily unavailable, the server returns a stable error, leaves the cookie/session available, and lets the
user retry instead of losing the only stored revocation credential before it can be revoked. Successful revocation removes
the OAuth scopes granted to this project and invalidates its issued access and refresh tokens; it does not sign the
user out of their Google account itself.

The Google and Firestore SDKs are isolated behind ZIO service interfaces.
``AppConfig`` loads and validates deployment settings as an effect;
``GoogleOAuth``, ``SessionStore``, and ``TokenGenerator``
are supplied through layers. External clients are scoped resources and are
closed automatically when the application stops. Google ``ApiFuture`` values
are bridged asynchronously into interruptible ZIO effects instead of blocking
a worker thread with ``Future.get``.

The backend reaches Google Cloud through Application Default Credentials, so
no code changes are needed between environments:

* Locally, log in manually (outside the build) with
  ``gcloud auth application-default login
  --impersonate-service-account=<GCP_PROJECT_ID's Firestore service account>``.
* On Cloud Run, give the service's runtime identity equivalent Firestore permissions.

The GCP project, Firestore database ID and location, deployment details, origins, and secret-file paths all live
in the external shared configuration rather than Scala source or tracked environment files:

* ``sbt run`` sources the environment-neutral ``backend/src/main/resources/test.env`` into the forked local
  process, adds the shared GCP/Firestore settings, and injects ``LOCAL_BASE_URL`` as the backend's
  ``PUBLIC_BASE_URL`` together with its port as ``PORT``. When Debug is enabled, the build also supplies
  ``ADMIN_PASSWORD`` without modifying ``test.env``.
* ``sbt artifact`` instead reads ``backend/src/main/resources/prod.env`` and appends the OAuth configuration,
  optional admin password, and ``ARTIFACT_BASE_URL`` (renamed to ``PUBLIC_BASE_URL`` for the backend) to the
  generated ``prod.env`` staged in the Docker build context. The source ``prod.env`` remains free of secrets and
  deployment addresses; the image's ``runApp`` launcher sources the generated copy at startup.
* ``sbt deployGCloud`` builds from the same non-secret source ``prod.env`` without appending those values. It
  supplies the shared GCP/Firestore settings, ``PUBLIC_BASE_URL``, and runtime credentials to the Cloud Run
  revision at deployment time, and relies on workload identity for Google Application Default Credentials.

In every mode nothing needs to be set by hand at run time.

Startup deliberately touches Firestore's *admin* API not at all. The backend assumes its database already
exists and that the TTL policy below is already in place, so a cold start builds only the data client and
begins serving — no admin gRPC channel, no ``GetDatabase``, no ``GetField``. This app is expected to scale to
zero and restart often, and admin round trips are pure latency on every one of those starts. The cost is that
creating the database and setting its TTL policy are genuinely manual, one-time steps: nothing repairs them at
runtime.

All apps under this GCP project share the single ``(default)`` Firestore database. ``FIRESTORE_DATABASE_ID``
still exists so the backend never guesses, but ``(default)`` is the expected value.

One-time setup:

1. In the Google Cloud console of the target project, create an OAuth 2.0
   web client and register every callback URI the app will use, for example
   ``http://localhost:<local-port>/auth/callback`` and
   ``https://<service>.run.app/auth/callback``. Each must exactly match the corresponding shared base-URL
   setting with ``/auth/callback`` appended.
2. Put ``OAUTH_CONFIG_PATH`` and the remaining settings in the external ``config.env`` described above. The build
   injects the OAuth values into the local process or deployment without modifying tracked files.
3. Add one ``APPCONFIGPATH`` locator under ``.local/`` on each development machine.

Each document in the ``Access`` collection represents one browser session. Its
document ID is a random 256-bit URL-safe session key, which is also stored in
the ``sessionKey`` field. The other fields are ``email``, ``name``,
``createdAt``, and ``expiresAt``. If Google returns an OAuth refresh token, it
is stored as ``refreshToken``. Otherwise the short-lived access token is stored as ``accessTokenForRevocation``
solely so Logout can revoke the grant; the two fields are never populated together. A protected
request is authenticated by looking up the cookie's session key and rejecting
missing or expired records. A successful logout deletes the current document, revoking only that browser session,
after revoking the associated Google grant. There is no process-global encryption key and backend restarts do not
invalidate sessions; Firestore is the durable session store. Treat document
IDs, ``sessionKey``, and ``refreshToken`` values as secrets.

``Access.expiresAt`` needs a Firestore TTL policy so that Firestore removes expired session documents
asynchronously, rather than retaining every rejected session forever. Set it once per project, by hand:

.. code-block:: console

   gcloud firestore fields ttls update expiresAt --collection-group=Access --database='(default)' --enable-ttl

Nothing checks or repairs this at runtime (see the startup note under `Login with Google`_), so a project where
it was never run accumulates dead session documents indefinitely. It is idempotent, and its server-side backfill
runs in the background. The emulator has no TTL support and needs nothing: its data is in-memory and is cleared
whenever it restarts.

A missing or expired session produces ``401 Unauthorized``. A Firestore error
produces ``503 Service Unavailable`` and is logged, rather than being presented
to the frontend as an unauthenticated session.

The session cookie is HttpOnly, ``SameSite=Lax``, scoped to ``/``, and expires
after seven days. It is marked Secure when the callback is served over HTTPS.
The OAuth ``state`` cookie has the same browser protections and expires after
ten minutes. Successful logout explicitly expires both cookies using their original paths.

Testing against a local Firestore emulator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``test.env`` sets ``FIRESTORE_EMULATOR_HOST=localhost:8880``, so ``sbt run`` talks to a local Firestore emulator
instead of the real GCP project by default. The Firestore client library detects this environment variable
itself and connects to that local, unauthenticated instance instead — skipping Application Default Credentials
entirely, so the ``gcloud auth application-default login`` step above isn't needed for local runs. Remove or
comment out the line in ``test.env`` to go back to hitting the real project locally.

``firebase.json`` at the repository root configures the emulator's port (``8880``), enables its web UI
(``4000``), and sets ``singleProjectMode: false``. Start it through sbt (install the Firebase CLI with
``npm install -g firebase-tools`` first if needed):

.. code-block:: console

   sbt firestoreEmulator

The task reads ``GCP_PROJECT_ID`` from the shared configuration, copies ``firebase.json`` into a generated
``target/firebase/`` working directory, writes a matching generated ``.firebaserc`` there, and invokes Firebase
with an explicit ``--project`` argument. No project-specific Firebase file is committed. The E2E orchestration
uses the same staging strategy.

With it running, browse ``http://localhost:4000/firestore`` to inspect the ``Access`` collection live while
exercising the login flow. Restarting the emulator wipes its data (it's in-memory only), so a fresh restart
means signing in again before there's anything to see.

Getting the Emulator UI to actually display data here took two separate fixes, each worth knowing about since
the failure mode of each is "looks fine, shows nothing," with no error surfaced anywhere obvious:

* ``singleProjectMode: false`` in ``firebase.json``: the Emulator UI defaults to "demo mode," which only
  recognizes a single project.
* The generated ``.firebaserc``: without it, the UI's own browser-side code resolves its *own* project id
  independently of both the app and any ``--project`` flag, falling back to a synthetic ``demo-no-project`` — so
  it queries a project with nothing in it while the real data sits under ``GCP_PROJECT_ID``. This one is diagnosable by
  opening the browser's network tab and checking which project id the ``listCollectionIds``/data requests to
  ``localhost:8880`` actually use.

Data can be genuinely present and readable — verifiable directly against the emulator's REST API, e.g. ``curl
http://localhost:8880/v1/projects/<project-id>/databases/(default)/documents/Access`` — while the UI
shows nothing, for either of the two reasons above.

The emulator implements Firestore's data API but not its admin API, so a request like ``GetDatabase`` answers
``UNIMPLEMENTED``. Nothing in this app calls that API any more, which is why no such warning appears at
startup.

Google service entitlements
----------------------------

This application deliberately requests no Google API scopes beyond ``openid email profile``. Asking for anything
more subjects the OAuth consent screen to Google's sensitive-scope verification review, which this app has no
reason to take on. ``GOOGLE_SERVICES`` is consequently absent from both ``test.env`` and ``prod.env``, left as a
commented-out line documenting the mechanism.

The mechanism itself still works. ``GOOGLE_SERVICES`` is an optional comma-separated list of OAuth scope URLs;
missing or empty requests no additional entitlements, while a value is appended to the ``scope`` parameter by
``GoogleOAuth.authorizationUrl``. Google shows a consent screen the first time a user grants a given scope set.

``access_type=offline`` is requested on every login regardless of scopes, so Google still issues a refresh token
for the base scopes alone. It is stored on the browser-session document alongside ``email``/``name`` (see
`Login with Google`_) and resolved onto ``SessionUser.refreshToken`` by ``SessionStore.find``. Two things depend
on it today: ``Logout``'s revocation of the Google grant, and session renewal, which exchanges it for a
short-lived access token purely to confirm Google still honours the grant before extending the session. A route
needing to call a Google API on the user's behalf would authorize that call with the same
``GoogleOAuth.accessToken(refreshToken)`` — after adding its scope here, and accepting the review that implies.

Admin-protected routes
-----------------------

``AccessPolicy.AdminPassword`` requires a ``?pwd=`` query parameter equal to the ``ADMIN_PASSWORD`` environment
variable before the handler runs. A missing or incorrect password produces ``401 Unauthorized``; a missing or
unreadable ``ADMIN_PASSWORD`` produces ``503 Service Unavailable`` (fail closed rather than fall open).
``AuthenticatedAndAdminPassword`` composes that check with browser-session authentication.

The separately built ``Debug`` plugin uses ``AccessPolicy.AuthenticatedAndAdminPassword``, so reaching it requires
*both* a signed-in Google session and the correct password. To enable it, add this setting to the external
``config.env``:

.. code-block:: text

   ADMIN_PASSWORD_PATH=admin.pwd

The path may be absolute or relative to ``config.env``. The referenced password file must exist and contain a
non-empty password.

When configured, ``sbt run`` and backend test tasks add the standalone ``debugPlugin`` JAR to the backend's
runtime and test classpaths, root ``sbt test`` also runs the plugin's tests, and ``sbt artifact`` copies the JAR
into the Docker image. The password is supplied as
``ADMIN_PASSWORD`` alongside the values parsed from ``test.env`` for ``sbt run``, or appended to the generated
``prod.env`` for ``sbt artifact``; neither tracked env file is modified. Sign in and visit
``https://<host>/debug?pwd=<password>`` directly in the browser's address bar; there is intentionally no on-page
link or button to it. A plugin that an operator should reach without signing in would instead use
``AccessPolicy.AdminPassword``.

If the setting is absent or commented out, the plugin's JAR is absent from backend classpaths and deployment
artifacts, root tests skip its suite, and no admin password is read or injected. The project remains available
for an explicit ``sbt debugPlugin/packageBin`` command.

Debug enablement is evaluated when each relevant task runs, rather than when sbt loads ``build.sbt``. Editing or
commenting the setting therefore takes effect on the next ``clean``, ``run``, ``test``, or ``artifact`` command
in the same sbt session; no ``reload`` is required. Root ``clean`` also cleans the standalone plugin's output so
an old JAR cannot persist as linked state.

The password travels as a URL query parameter, so treat it like any other bearer credential: it can end up in
browser history and proxy or server access logs. Rotate ``admin.pwd`` and redeploy if it leaks.

**CAUTION:** The ``Debug`` module is not meant to be part of a *real* production
deployment. It is only meant to be useful as a means of exploring a prospective
deployment environment in the cloud, such as AppEngine, CloudRun, or Lambda,
in order to find out the underlying environment where the production artifact
will run. You may deploy a debug-enabled empty project into, say, Google Cloud's
App Engine to find out the size of the virtual machine it would run on, and how
nginx is configured as a reverse proxy. However, as you deploy your real production
app, make sure to configure the build to not include ``Debug``.

Adding a backend plugin
-----------------------

Place each plugin in a package below ``sgrv.be`` and make its top-level object implement ``BackendPlugin``. Its
abstract ``Requires`` type, runtime ``CapabilitySet``, access policy, and native ZIO HTTP routes form one
compiler-checked contract. For example, an authenticated plugin needing the session store is:

.. code-block:: scala

   package sgrv.be.example

   import sgrv.be.BackendCapabilities
   import sgrv.be.auth.SessionStore
   import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
   import zio.http.{Method, Response, Routes, handler}

   object Example extends BackendPlugin:
     type Requires = SessionStore

     override val id = "example"
     override val requirements: CapabilitySet[Requires] =
       CapabilitySet.one(BackendCapabilities.sessionStore)
     override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
     override val routes: Routes[Requires & RequestContext, Nothing] =
       Routes(Method.GET / "example" -> handler(Response.text("example")))

Combine requirements with ``++``: a ``CapabilitySet[A]`` plus a ``CapabilitySet[B]`` has the type
``CapabilitySet[A & B]``. Resolution returns ``ZEnvironment[A & B]``; if either capability is absent, the plugin
is skipped with the missing capability IDs. The plugin's ``routes`` require ``Requires & RequestContext``: route
discovery supplies the request context after applying the policy, while using an undeclared capability service is
a compile-time error. ``AccessPolicy`` is contravariant, allowing ``Public`` or a policy requiring only a subset
of the plugin environment. Authenticated handlers can obtain the already-resolved user by reading
``RequestContext`` and matching ``RequestContext.Authenticated``.

Keep plugin-specific services inside the plugin JAR. A plugin calling an external HTTP API would require the
generic ``BackendCapabilities.httpClient`` capability and construct its own private adapter from it; the host
would neither register nor depend on an API-specific capability.

The available policies are ``Public``, ``Authenticated``, ``AdminPassword``, and
``AuthenticatedAndAdminPassword``. ClassGraph discovers implementations of the nominal JVM interface; there is
no reflective cast to a generic Scala function. Plugin IDs and API versions are validated, and duplicate route
patterns (including collisions with static routes) reject the involved plugin deterministically.

Reacting to a session starting or ending
----------------------------------------

Some work belongs to the fact that a session began or ended rather than to any endpoint the browser calls. The
host raises lifecycle events for that, and ``SessionListener`` is the extension point that consumes them — the
same shape as `Adding a backend plugin`_, but triggered by an event instead of a route:

.. code-block:: scala

   package sgrv.be.example

   import sgrv.be.BackendCapabilities
   import sgrv.be.core.{CapabilitySet, LoginEvent, LogoutEvent, SessionListener}
   import com.google.cloud.firestore.Firestore
   import zio.ZIO

   object Example extends SessionListener:
     type Requires = Firestore

     override val id = "example"
     override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.firestore)

     override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit
     override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

Both hooks default to doing nothing, so a listener implements only the half it cares about.

Listeners are discovered by the same ClassGraph scan as route plugins (``ModuleDiscovery``), resolve their
capabilities the same way, and are validated the same way: an invalid id, an incompatible ``apiVersion``, or a
missing capability is reported and the listener is left out. ``Main`` builds them into a ``SessionNotifier``
before route discovery runs, then adds that notifier to the capability registry, so ``Callback`` and ``Logout``
require it as the ``session-notifier`` capability like any other service. Neither flow names its listeners.

Both events carry the ``SessionUser``, the time, and the opaque ``sessionKey``. The session key is included
because it is the only stable identifier of one session, which lets a listener key its own records to it — a
reissued cookie means a new session and a new record. It is a secret: derive from it (hash it) rather than
copying it into another collection.

Four properties are deliberate:

* **A listener cannot fail a login or a logout.** Its error is logged and the flow still completes.
  Authentication does not depend on what a listener wanted to do about it. Flip this only with a clear answer for
  what the user should see when the side effect fails but their credentials were fine.
* **Listeners run before the response**, in ``id`` order, so a record a listener creates already exists by the
  time the browser loads the home page. A slow listener therefore delays the response.
* **They run once per session change**, not per page load, because the events are raised in ``/auth/callback``
  and ``POST /logout``.
* **The logout event is raised only after the logout really happened** — after Google revocation and after the
  browser session is deleted — so a listener never records an ending that did not occur.

``sgrv.be.sessions.CountingSessionListener`` is the worked example. A login opens nothing: every device that
signs in reaches the same screens and only one of them counts, so a counting session is opened when a device takes
the counter's role instead. A logout, from any device, closes the account's session in progress and records why
(``endedBy``), leaving it in place as a workout in the history. The same object derives a browser's identity from
its session key by hashing, as recommended above, and ``CountingSessionContributor`` reports that identity on
``/me``.

Adding to the /me response
--------------------------

``/me`` is the one call the frontend makes on every page load, so anything it needs about the current session can
ride along on it instead of costing a second round trip. ``CurrentUserContributor`` is the extension point for
that, discovered and validated exactly like ``SessionListener``:

.. code-block:: scala

   object Example extends CurrentUserContributor:
     type Requires = Any

     override val id = "example"
     override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
     override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
       ZIO.some(Json.Obj("hello" -> Json.Str(context.user.email)))

Each contribution is filed under its contributor's id, so two can never collide:

.. code-block:: json

   {"email": "...", "name": "...", "extra": {"example": {"hello": "..."}}}

This app's own contribution is ``counting-session``: the digest of the browser's session key that the account's
record names as its counter while that browser is counting.

``CurrentUser.extra`` is absent rather than empty when nothing contributes, so an application with no
contributors sees exactly the payload this route has always returned. A contributor may return ``None`` to add
nothing for a given request, and a failing contributor omits only its own key — resolving who is signed in never
depends on what an application wanted to say about them.

Logging
-------

Every HTTP request is logged by a ZIO HTTP handler aspect. Console output uses a
compact format with a millisecond timestamp and request summary:

.. code-block:: text

   2026-08-01T03:29:51.927 INFO Http request served [GET /debug -> 200 70ms]

The logger accepts ``TRACE`` and higher levels. It deliberately omits fiber IDs
and request and response sizes from the text output.

Compiler hygiene and formatting
--------------------------------

Every Scala subproject enables deprecation, feature, unchecked, unused-code, and discarded-value warnings.
SemanticDB is also enabled so Scalafix can apply semantic rules. Format the whole repository and remove unused
code with:

.. code-block:: console

   sbt fmt

Verify that Scalafix and Scalafmt would make no changes with:

.. code-block:: console

   sbt fmtCheck

The aliases explicitly include the separately packaged Debug plugin and E2E project as well as the projects
aggregated by the root build.

Tests and coverage
------------------

Run all tests with:

.. code-block:: console

   sbt test

``sbt test`` at the root also runs the frontend's Scala.js tests. When ``ADMIN_PASSWORD_PATH`` is configured in
the shared config, it additionally runs the Debug-plugin tests; without that opt-in the root build ignores the plugin.
The frontend tests need Node.js installed; without it, run ``sbt backend/test`` and, when enabled,
``sbt debugPlugin/test``.

The backend and Debug-plugin tests cover server configuration and static assets; nominal plugin discovery; typed
intersection capability resolution; missing-capability skips; access-policy gating; API incompatibility,
activation-failure, and route-conflict isolation; request-log formatting; debug signature generation; OAuth
configuration and URL generation (including ``GOOGLE_SERVICES`` parsing and the resulting scope list), user-name
fallback, authentication JSON, logout revocation/invalidation ordering and cookie expiry, session-renewal outcomes,
and discovery of every route; and, for the app's own routes, the account key, the idle rule, the validation of
progress reports, recordings, settings and test events, and that every field of a shared type is written. They use
deterministic test data; they do not call Google or a live Firestore API, so the Firestore queries themselves are
exercised only by running the app.

The frontend tests run the detector against synthetic signals, several of them built to reproduce failures first
seen in recorded traces (the filter, peak detection, the tally and its corroboration, the rest phase and the
signal-strength bands), and cover the camera's settle-and-hold rules, the dashboard's arithmetic (pace, projections, calories, factors), the CSV export, the
stored count and start-up preference, the frontend state's persistence, and the bench's motion and scoring.

Generate an scoverage report for the backend with:

.. code-block:: console

   sbt clean "project backend" coverage test coverageReport

Coverage instrumentation is disabled for the root project and Scala.js frontend.
The generated HTML report is
``backend/target/scala-3.8.4/scoverage-report/index.html``.

End-to-end tests (Selenium)
----------------------------

``e2etest`` is a separate sbt project (``e2etest/src/test/scala/``) that drives the real, running application
through an actual Chrome instance via Selenium, rather than calling backend code directly the way the unit
suite does. Run it with:

.. code-block:: console

   sbt e2etest/test

That single command does more than run tests — it's a full orchestration, defined by overriding ``e2etest``'s
``Test / test`` task in ``build.sbt``:

1. Launches a headless Chrome via Selenium (Selenium Manager auto-resolves a matching chromedriver; only a real
   Chrome install is required) and immediately quits it, failing fast with a clear message if that doesn't work,
   rather than failing confusingly partway through the first real test.
2. Checks the port from ``LOCAL_BASE_URL`` isn't already in use — by a leftover Docker container from manual testing,
   say — and fails
   loudly if it is, rather than the next step silently exercising and recording coverage for the wrong process.
3. Starts the backend in the background via a *nested* ``sbt "project backend" coverage run``, mirroring the
   ``coverage``/``coverageReport`` workflow above so the HTTP traffic these tests generate is scoverage-
   instrumented and recorded exactly like the unit suite's own coverage, accumulating into the same measurement
   data. Its output is captured to ``e2etest/target/e2e-backend.log``.
4. Polls the backend until it responds (up to 90s) before running anything against it.
5. Runs the actual Selenium suite.
6. Stops the backend regardless of whether the tests passed, walking the *entire* process tree (the ``run /
   fork := true`` backend forks its own child JVM, so stopping just the nested sbt process it started as would
   leave that JVM orphaned and still bound to the port).

Since Google blocks WebDriver-controlled browsers from driving its login form (see `Authenticated E2E tests`_
below), ``e2etest/test`` itself only covers what's reachable while signed out — ``sgrv.e2e.HomePageE2ESuite``
loads the home page and asserts the Google login link is present. It runs every suite it discovers, so
``sgrv.e2e.SignedInE2ESuite`` is discovered here too; finding no signed-in browser on its debugger port, that
suite skips its tests rather than failing them. ``coverageReport`` is deliberately *not*
triggered automatically by either ``e2etest/test`` or ``testAuthenticated`` — run it yourself, as a separate
step, once you've run whichever combination of ``coverage test`` (unit), ``e2etest/test`` (signed-out E2E),
and/or ``testAuthenticated`` (signed-in E2E) you want reflected; scoverage's measurement data accumulates
additively across runs until something explicitly cleans ``backend/target``, so a single ``coverageReport``
afterward can combine all of them into one report rather than any one overwriting another's data.

Authenticated E2E tests
~~~~~~~~~~~~~~~~~~~~~~~~

**Why login isn't automated.** Google actively detects and blocks sign-in attempts from WebDriver-controlled
browsers (the interactive sign-in form specifically, not merely an already-authenticated session) — this is a
deliberate anti-automation measure on Google's end, not something specific to this app or fixable by using an
existing Chrome profile (that runs into its own problems: a profile already open in your regular Chrome can't
also be opened by ChromeDriver, and Chrome's saved-password autofill isn't something WebDriver can drive anyway,
since it's native browser UI rather than part of the page's DOM). Nor does Google Cloud or Firebase offer an
emulator for this: ``gcloud emulators`` covers Firestore/Datastore/Bigtable/Pub/Sub/Spanner only, and while the
separate Firebase Local Emulator Suite does have an Authentication Emulator, it only intercepts calls made
through the *Firebase Auth SDK* — this app implements the OAuth 2.0 flow directly
(`Login with Google`_), so there's nothing for it to intercept.

Instead, sign in once by hand and let a later test run reuse that session. Start with:

.. code-block:: console

   sbt e2etest/launchTestBrowser

This leaves three things running (rather than tearing them down the way ``e2etest/test`` does), starting
whichever of them isn't already up:

1. The local Firestore emulator (``localhost:8880`` per ``test.env``). Sessions are stored there (see
   ``SessionStore`` in `Testing against a local Firestore emulator`_), and since the emulator is in-memory only,
   it has to stay running continuously from the sign-in below through to a later ``testAuthenticated`` run, or
   the session is lost.
2. The backend, coverage-instrumented (``sbt "project backend" coverage run``), pointed at that emulator.
3. A visible, remote-debuggable Chrome (``--remote-debugging-port=9222``, using a dedicated profile under
   ``e2etest/target/test-chrome-profile`` rather than your everyday one — Chrome refuses to open a profile
   twice, and leaving debugging enabled on your daily-driver profile would let anything on the machine attach to
   it), opened to the backend's configured ``LOCAL_BASE_URL``. Use ``localhost``, not ``127.0.0.1``: visiting through the latter
   would leave the OAuth state cookie on a different origin, and the callback would correctly reject it.

Running ``launchTestBrowser`` again reuses whichever of the three are already up rather than starting duplicates.
Once it's ready, switch to that Chrome window and sign in with Google as you normally would, then leave that
window, the backend, and the emulator all running.

With that session live, run:

.. code-block:: console

   sbt e2etest/testAuthenticated

This fails immediately, with a clear message, if the backend or test browser aren't already up — unlike
``e2etest/test``, it never launches or tears down either itself. It attaches Selenium to the running Chrome via the
Chrome DevTools Protocol's ``debuggerAddress`` option instead of launching a fresh browser (calling
``ChromeDriver.quit()`` on such an attached session only ends that WebDriver session; it does not close the real
browser window), and runs ``sgrv.e2e.SignedInE2ESuite``: it opens About from the menu and asserts it shows the
signed-in account and every build-information field, then goes to the role picker and walks each role — into the
dashboard and back, then into the counter, taking over if another device holds the role, and back. Entering the
counter is real: it takes the role for the signed-in account and opens a workout in its history. Since it attaches to
the same coverage-instrumented backend ``launchTestBrowser`` started, this traffic accumulates into the same
measurement data as everything else.

Rendering README.rst to HTML
-----------------------------

.. code-block:: console

   sbt readmeToHtml

Renders this file to ``target/README.html`` via `docutils <https://docutils.sourceforge.io/>`_
(``python3 -m docutils README.rst target/README.html``), for previewing it outside of whatever renders
``.rst`` for you natively (e.g. GitHub). Requires Python 3 with the ``docutils`` package installed
(``pip install docutils``); fails with a clear message if either is missing.

Packaging and deployment
------------------------

Build a Docker image with:

.. code-block:: console

   sbt artifact

This performs a clean build, stages a Docker build context under ``backend/target/docker/`` (application JAR, all
runtime dependency JARs, a generated ``prod.env`` with the shared runtime configuration, OAuth client
configuration, selected public origin, and ``ARTIFACT_PORT``, the ``runApp`` launcher, and the ``Dockerfile``
itself), and runs ``docker build`` there (assumed already installed). If Debug is enabled, its JAR and admin
password are included too. The result is tagged both
``cadencecam:<version>`` and ``cadencecam:latest``.

``dockerPlatform`` near the top of ``build.sbt`` (default ``linux/amd64``) sets the image's target platform
independently of the machine running the build — e.g. building on Apple Silicon for an amd64 deployment host.
Docker/BuildKit cross-builds via emulation as needed, so nothing else has to change; it's slower than a native
build (and a container started from a foreign-platform image runs under emulation too, noticeably slower to
start than a native one) but produces a correct image either way.

The image declares ``SIGTERM`` as its stop signal and its exec-form ``ENTRYPOINT`` runs ``runApp``. After loading
``prod.env``, that launcher uses ``exec java`` so the JVM replaces the shell and runs as PID 1. Docker or a
container orchestrator can therefore deliver ``SIGTERM`` directly to ZIO's shutdown hook instead of relying on
a shell to forward it. The hook interrupts the HTTP server, which stops normally and gives in-flight requests up
to eight seconds to complete; the whole ZIO application has a nine-second shutdown budget. Configure the
container runtime to allow at least that long before escalating to ``SIGKILL``.

Google Cloud Run
~~~~~~~~~~~~~~~~

Deploy the application with:

.. code-block:: console

   sbt deployGCloud

The task performs a clean build, creates a separate Cloud Run Docker context under
``backend/target/docker-gcloud/``, authenticates Docker to Artifact Registry, pushes
``<region>-docker.pkg.dev/<project-id>/<repository>/cadencecam:<version>``, and deploys the public
``cadencecam`` service in the configured region on port ``8080``. It reads the project, region, repository,
and runtime service account from the shared configuration. Docker and an authenticated ``gcloud`` CLI must be
available locally. The deploying account needs permission to push to that repository and update Cloud Run. The
configured runtime identity separately needs the Firestore permissions used by the backend, and the deploying
user needs ``roles/iam.serviceAccountUser`` on it.

The service is deployed with ``--max-instances 1``. The cap was needed while live readings were relayed through
an instance's memory, where a counter and its dashboard landing on different instances heard nothing from each
other. Readings now travel directly between the devices and every request reads its state from Firestore, so nothing
in the design depends on a single instance any more; the cap also bounds how many instances a burst of traffic can
start.

The service answers at https://cadencecam.raz.sg, a domain mapped to it in Cloud Run and set as ``PUBLIC_BASE_URL``.

The staged runtime libraries include the packaged ``sharedJVM`` project explicitly. Inter-project sbt
dependencies otherwise appear on the backend runtime classpath as class directories rather than JAR files and
would be lost when assembling the Docker context.

The Cloud Run image is deliberately secret-free. Its ``prod.env`` contains only committed, environment-neutral
Google-service settings, its ADC directory is empty, and GCP/OAuth/admin values never enter the Docker context.
The OAuth client secret is still required by the running application to exchange authorization codes, so
``deployGCloud`` supplies it—along with the client ID, selected ``PUBLIC_BASE_URL``, and optional Debug password—
to the Cloud Run revision from a temporary YAML file that is deleted after the command finishes. Google API and
Firestore calls use the Cloud Run service's workload identity instead of local ADC.

The Cloud Run origin comes from ``PUBLIC_BASE_URL`` in the shared configuration; the standalone artifact's
separate ``ARTIFACT_BASE_URL`` has no influence on this task. It must be the address through which
browsers actually reach this Cloud Run deployment, and its exact
``/auth/callback`` URL must be registered on the Google OAuth client. On the first deployment, ``gcloud`` reports
the newly assigned service URL; register it, activate it as ``PUBLIC_BASE_URL``, and deploy again unless a custom
domain was already selected.

The HTTP server binds to ``BIND_ADDRESS`` (``127.0.0.1`` if unset) — a process bound only to loopback is
unreachable from outside a container regardless of published ports, so the ``Dockerfile`` sets
``BIND_ADDRESS=0.0.0.0`` itself. Run the image with:

.. code-block:: console

   docker run -p <host-port>:<artifact-port> cadencecam:latest

or, if a reverse proxy (e.g. nginx) on the same host will terminate HTTPS and forward to it, publish only to
loopback so nothing else on the network can reach the container directly:

.. code-block:: console

   docker run -p 127.0.0.1:<host-port>:<artifact-port> cadencecam:latest

Application Default Credentials work differently depending on where the image runs. On Cloud Run/GKE/GCE, an
image built without a local ADC file falls through to the platform's workload identity (the attached service
account), exactly as described in `Login with Google`_. If a local file is bundled, Google's well-known-file
lookup precedes workload identity, so use an artifact built without that file when the platform identity should
apply. Running the image *outside* a GCP platform — a plain ``docker run`` on any other host, including for local
testing — needs credentials from somewhere, since there's no metadata server to ask;
``sbt artifact`` bakes in whatever ADC file it finds at gcloud's own well-known location
(``~/.config/gcloud/application_default_credentials.json`` on macOS/Linux, generated by
``gcloud auth application-default login``) if present, warning and proceeding without it otherwise.
The Docker build context always contains an ``adc/`` directory, so its ``COPY`` remains valid when that directory
is empty. The image deliberately does not set ``GOOGLE_APPLICATION_CREDENTIALS``: an included file is discovered
at gcloud's well-known path, while an absent file leaves ADC free to fall through to workload identity or another
provider.

This makes the standalone ``sbt artifact`` image itself a secret-bearing artifact, on top of the OAuth secret and
any conditionally included admin password baked into its copy of ``prod.env``. Do not push it to a public
registry; transfer it directly with ``docker save``/``docker load``, or push to a private registry you control.
``sbt deployGCloud`` deliberately builds a different, secret-free context for Artifact Registry. The standalone
ADC mechanism exists for testing the exact local image or running it on a host with no workload identity, such
as a plain Linux VM behind your own reverse proxy.

Repository layout
-----------------

.. code-block:: text

   build.sbt                                   the build, including the run, artifact and deployGCloud tasks
   project/                                    build helpers: shared configuration, OAuth, account key, origins
   firebase.json                               the local Firestore emulator
   shared/src/main/scala/sgrv/api/             every shape and path the two ends exchange
     Api.scala                                   /me, /about, progress reports, signal recordings
     Live.scala                                  readings, commands and pairing between devices; presence;
                                                 the bench's test events and run discards
     Settings.scala                              the account's weight and exercises; deleting its data
     Workouts.scala                              the history, and deleting one workout
     Documents.scala                             the privacy policy and the terms
   frontend/src/main/scala/sgrv/fe/
     Main.scala                                  the application: every screen and panel, and start-up
     FrontendState.scala                         what the browser persists, and the screens
     Effort.scala                                the dashboard's arithmetic: pace, calories, projections
     Menu.scala, Readouts.scala                  pieces several screens share
     StartPreference.scala                       the device's "default to dashboard" preference
     WorkoutCsv.scala                            the history as a CSV file
     Device.scala, HttpService.scala             the handset's name; every request's way out
     acquire/                                    the counter: camera, sampling, detector, reporting
     live/PeerLink.scala                         the direct link between a counter and its watchers
     bench/                                      the test bench behind #test
     refreshstate/                               session renewal, independent of the application
   backend/src/main/scala/sgrv/be/
     Main.scala                                  static routes, discovery, and the server
     BackendEnvironment.scala                    the host's capabilities
     auth/                                       Google login, browser sessions, /me, logout, renewal
     core/                                       plugin, listener and contributor discovery; access policies
     store/                                      the Firestore client
     about/                                      GET /about
     sessions/                                   the app's routes: accounts, counting sessions, history,
                                                 settings, pairing, recordings, test events
   backend/src/main/resources/
     prod.env, test.env                          environment-neutral runtime settings
     Dockerfile, runApp                          the image and its launcher
     web/                                        index.html, style.css, manifest, icons, privacy.html, tos.html
   backend/src/test/scala/                     backend tests
   frontend/src/test/scala/                    frontend tests
   debug-plugin/                               the optional, separately packaged Debug plugin
   e2etest/src/test/scala/                     Selenium suites
   scripts/icon/                               the icon's SVG sources and how the PNGs are made from them
   scripts/stopapp.ps1                         stops a local server by port, on Windows

Forking this repository
-----------------------

Forking this repository to start a new project means replacing every piece of data specific to *this*
deployment — a GCP project, an OAuth client, a few secret files, a handful of settings — while the infrastructure
described above (plugin discovery, capability resolution and access policies, the session store, session renewal)
keeps working unchanged underneath your own routes. CadenceCam itself is the part to replace: ``sgrv.be.sessions``,
the frontend outside ``refreshstate``, and the app's shapes in ``sgrv.api``.

What absolutely needs changing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. **A Google Cloud project of your own**, with the Firestore API enabled, its ``(default)`` database created,
   and the ``Access.expiresAt`` TTL policy set — all one-time console/CLI steps, since the backend deliberately
   does none of them at startup (see `Login with Google`_). Every Google API is enabled and billed
   independently, so any scope you later add to
   ``GOOGLE_SERVICES`` also needs its API turned on for the project — calls fail with ``403`` if it isn't, even
   though the OAuth scope was granted.

2. **A new OAuth 2.0 web client**, created in that project, with your own callback URIs registered (step 1 under
   `Login with Google`_'s one-time setup). Download its JSON as your fork's ``oauth.config.json`` and keep it
   outside source control. The build is intentionally unusable until the external configuration and its
   compulsory ``OAUTH_CONFIG_PATH`` resolve successfully.

3. **External build configuration** — create the shared ``config.env`` described under `OAuth configuration file`_
   and point each clone at it with one ``APPCONFIGPATH=...`` locator under ``.local/``. The tracked
   ``backend/src/main/resources/prod.env`` and ``test.env`` contain only environment-neutral settings:

   * ``LOCAL_BASE_URL``, ``ARTIFACT_BASE_URL``, and ``PUBLIC_BASE_URL`` — supply all three through the shared
     configuration rather than either committed env file. They select the origin for ``run``,
     ``artifact``, and ``deployGCloud`` respectively. Register each selected origin's exact ``/auth/callback`` URI
     on the Google OAuth client.
   * ``GCP_PROJECT_ID``, ``FIRESTORE_DATABASE_ID`` — your new project and its database, normally ``(default)``,
     which every app under that project shares. The backend never creates it.
   * ``ACCOUNT_KEY_PATH`` — a fresh random key of your own, if you keep account-keyed data (see `Data model`_).
   * ``GCLOUD_REGION``, ``ARTIFACT_REGISTRY_REPOSITORY``, and ``GCLOUD_SERVICE_ACCOUNT`` — the Cloud Run target.
   * ``ARTIFACT_PORT`` — the port baked into a standalone artifact. ``LOCAL_BASE_URL`` supplies the local run
     port; Cloud Run supplies its own ``PORT``. The backend never silently chooses a port.
   * ``GOOGLE_SERVICES`` — the scopes your fork's own routes need (see `Google service entitlements`_). This
     app leaves it unset; set it only once a route calls a Google API beyond login.

4. **Optional Debug configuration** — if you keep the Debug plugin, create a fresh random password file outside
   source control and point to it with ``ADMIN_PASSWORD_PATH`` in the shared configuration (see
   `Admin-protected routes`_). Omit or comment out that setting to leave Debug out of normal builds.

5. **Application Default Credentials for the new project** (`Login with Google`_): locally,
   ``gcloud auth application-default login --impersonate-service-account=<new-project's-Firestore-service-account>``;
   in deployment, run the service under that same service account.

Worth changing, but not load-bearing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``ThisBuild / organization`` / ``organizationName`` and the ``name`` settings in ``build.sbt``
  (``cadencecam``, ``cadencecam-shared``, ``cadencecam-frontend``, ``cadencecam-backend``,
  ``cadencecam-debug-plugin``, ``cadencecam-e2etest``). The root project's ``name`` is not cosmetic: it becomes
  the Docker image tag *and* the Cloud Run service name, so changing it deploys a new service at a new URL.
* The ``<title>`` and Apple app title in ``backend/src/main/resources/web/index.html``, plus ``name``,
  ``short_name``, ``description``, and colors in ``backend/src/main/resources/web/manifest.webmanifest`` — these
  control the installed app's identity and launch appearance. Replace the favicon and the four PNG icons as a set;
  ``scripts/icon/`` shows how this app's were made.
* The ``sgrv.be`` / ``sgrv.fe`` package names. Purely a naming choice, but if you rename them, also update
  ``RouteDiscovery.discover``'s ``.acceptPackages("sgrv.be")`` filter in
  ``backend/src/main/scala/sgrv/be/core/RouteDiscovery.scala`` to match — otherwise route discovery silently
  finds nothing under the new package.

What to keep, drop, or extend
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Treat ``backend/src/main/scala/sgrv/be/auth/`` and ``.../core/`` as infrastructure: the OAuth flow, session
store, and route discovery/gating work as-is and shouldn't need edits unless you're changing how authentication
itself works. The standalone ``debugPlugin`` project and the ``about`` plugin, by contrast, are worked
*examples* — delete either if your project has no use for it, or use them as templates for your own plugins.

`Adding a backend plugin`_ above is the generic recipe for a new route; the routes already in the repository are
worked examples of the shapes a new route is likely to take:

* **A route with no session at all** — ``sgrv.be.auth.Login`` / ``Callback`` (``AccessPolicy.Public``). Copy this shape
  only if you're adding another pre-authentication entry point, which is uncommon.
* **The common case: an authenticated route whose handler doesn't need to know who's signed in** — the
  ``Example`` plugin under `Adding a backend plugin`_. It uses ``AccessPolicy.Authenticated`` and contains no
  authentication code in its route; a request only reaches the
  handler once route discovery has already confirmed a valid session. This is the right starting point for most
  new routes.
* **A route that must serve signed-in and signed-out requests differently** — ``sgrv.be.auth.Me``
  (``AccessPolicy.Public``, then the handler calls ``SessionAuth.resolve`` itself to tell the two cases apart, since the
  gate's generic ``401`` wouldn't distinguish "signed out" from "session lookup failed").
* **An authenticated route whose handler needs data *from* the session** — ``sgrv.be.auth.Logout``:
  ``AccessPolicy.Authenticated`` resolves the session once, then the handler reads the resulting
  ``RequestContext.Authenticated`` to reach ``SessionUser.refreshToken`` without a second Firestore lookup.
* **A route reachable by password instead of, or in addition to, a session** — the separately packaged
  ``sgrv.be.debug.Debug`` plugin uses ``AuthenticatedAndAdminPassword``; use ``AdminPassword`` for password-only access.
* **Calling a Google API on the user's behalf** — no route does this today, but the shape is fixed: add the
  scope(s) to ``GOOGLE_SERVICES``, require ``BackendCapabilities.httpClient``, and construct a plugin-private
  adapter over that generic ``zio.http.Client`` inside the plugin, authenticating calls with a Bearer access
  token from ``GoogleOAuth.accessToken``. No API-specific service is added to ``BackendEnvironment`` or
  ``Main``. Weigh the verification cost noted under `Google service entitlements`_ before adding a scope.
