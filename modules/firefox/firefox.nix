# Based on https://github.com/ghacksuserjs/ghacks-user.js/blob/bd384622db70eaf6893d32a0c1c4b99d5516fa5b/user.js
# Compare with master using https://github.com/ghacksuserjs/ghacks-user.js/compare/HASH1...HASH2

{
  # Enable webrender
  "gfx.webrender.all" = true;
  "gfx.webrender.enabled" = true;
  # Enable video hardware acceleration
  "media.ffmpeg.vaapi.enabled" = true;
  # Allow unsigned Add-ons. Or at least try...
  "xpinstall.signatures.required" = false;
  # Read userChrome.css and userContent.css
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  # Dark about:* pages.
  "browser.in-content.dark-mode" = true;
  # 0000: disable about:config warning
  "browser.aboutConfig.showWarning" = false;

  # [SECTION 0100]: STARTUP

  # 0101: disable default browser check
  "browser.shell.checkDefaultBrowser" = false;
  # 0102: set START page (0=blank, 1=home, 2=last visited page, 3=resume previous session)
  "browser.startup.page" = 0;
  # 0103: set HOME+NEWWINDOW page
  "browser.startup.homepage" = "about:blank";
  # 0104: set NEWTAB page
  "browser.newtabpage.enabled" = false;
  "browser.newtab.preload" = false;
  # 0105a: disable Activity Stream telemetry
  "browser.newtabpage.activity-stream.feeds.telemetry" = false;
  "browser.newtabpage.activity-stream.telemetry" = false;
  # 0105b: disable Activity Stream Snippets
  "browser.newtabpage.activity-stream.feeds.snippets" = false;
  "browser.newtabpage.activity-stream.asrouter.providers.snippets" = "";
  # 0105c: disable Activity Stream Top Stories, Pocket-based and/or sponsored content
  "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  "browser.newtabpage.activity-stream.showSponsored" = false;
  "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;
  "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
  # 0106: clear default topsites
  "browser.newtabpage.activity-stream.default.sites" = "";

  # [SECTION 0300]: QUIET FOX

  # 0302a: disable auto-INSTALLING Firefox updates
  "app.update.auto" = false;
  # 0306: disable extension metadata
  "extensions.getAddons.cache.enabled" = false;
  # 0310: disable sending the URL of the website where a plugin crashed
  "dom.ipc.plugins.reportCrashURL" = false;
  # 0320: disable about:addons' Recommendations pane (uses Google Analytics)
  "extensions.getAddons.showPane" = false;
  # 0321: disable recommendations in about:addons' Extensions and Themes panes
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
  # 0330: disable telemetry
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.server" = "data:,";
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  # 0331: disable Telemetry Coverage
  "toolkit.telemetry.coverage.opt-out" = true;
  "toolkit.coverage.opt-out" = true;
  "toolkit.coverage.endpoint.base" = "";
  # 0340: disable Health Reports
  "datareporting.healthreport.uploadEnabled" = false;
  # 0341: disable new data submission, master kill switch
  "datareporting.policy.dataSubmissionEnabled" = false;
  # 0342: disable Studies (see 0503)
  "app.shield.optoutstudies.enabled" = false;
  # 0343: disable personalized Extension Recommendations in about:addons and AMO
  "browser.discovery.enabled" = false;
  # 0350: disable Crash Reports
  "breakpad.reportURL" = "";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  # 0351: disable backlogged Crash Reports
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

  # [SECTION 0400]: BLOCKLISTS / SAFE BROWSING (SB)

  # 0401: enforce Firefox blocklist, but sanitize blocklist url
  "extensions.blocklist.enabled" = true;
  "extensions.blocklist.url" =
    "https://blocklists.settings.services.mozilla.com/v1/blocklist/3/%APP_ID%/%APP_VERSION%/";
  # 0412: disable SB checks for downloads (remote)
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";

  # [SECTION 0500]: SYSTEM ADD-ONS / EXPERIMENTS

  # 0503: disable Normandy/Shield
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
  # 0505: disable System Add-on updates
  "extensions.systemAddon.update.enabled" = false;
  "extensions.systemAddon.update.url" = "";
  # 0506: disable PingCentre telemetry (used in several System Add-ons)
  "browser.ping-centre.telemetry" = false;
  # 0518: disable Web Compatibility Reporter
  "extensions.webcompat-reporter.enabled" = false;

  # [SECTION 0700]: HTTP* / TCP/IP / DNS / PROXY / SOCKS etc

  # 0704: enforce the proxy server to do any DNS lookups when using SOCKS
  "network.proxy.socks_remote_dns" = true;

  # [SECTION 0800]: LOCATION BAR / SEARCH BAR / SUGGESTIONS / HISTORY / FORMS

  # 0801: disable location bar using search
  "keyword.enabled" = false;
  # 0802: disable location bar domain guessing
  "browser.fixup.alternate.enabled" = false;
  # 0803: display all parts of the url in the location bar
  "browser.urlbar.trimURLs" = false;
  # 0805: disable coloring of visited links - CSS history leak
  "layout.css.visited_links_enabled" = false;
  # 0807: disable live search suggestions
  "browser.search.suggest.enabled" = false;
  "browser.urlbar.suggest.searches" = false;
  # 0809: disable location bar suggesting "preloaded" top websites
  "browser.urlbar.usepreloadedtopurls.enabled" = false;
  # 0850e: disable location bar one-off searches
  "browser.urlbar.oneOffSearches" = false;
  # 0860: disable search and form history
  "browser.formfill.enable" = false;

  # [SECTION 0900]: PASSWORDS

  # 0901: disable saving passwords
  "signon.rememberSignons" = false;
  # 0912: limit (or disable) HTTP authentication credentials dialogs triggered by sub-resources
  "network.auth.subresource-http-auth-allow" = 1;

  # [SECTION 1200]: HTTPS (SSL/TLS / OCSP / CERTS / HPKP / CIPHERS)

  # 1201: require safe negotiation
  "security.ssl.require_safe_negotiation" = true;
  # 1204: disable SSL session tracking
  "security.ssl.disable_session_identifiers" = true;
  # 1205: disable SSL Error Reporting
  "security.ssl.errorReporting.automatic" = false;
  "security.ssl.errorReporting.enabled" = false;
  "security.ssl.errorReporting.url" = "";
  # 1206: disable TLS1.3 0-RTT (round-trip time)
  "security.tls.enable_0rtt_data" = false;
  # 1210: enable OCSP Stapling
  "security.ssl.enable_ocsp_stapling" = true;
  # 1211: control when to use OCSP fetching (to confirm current validity of certificates)
  "security.OCSP.enabled" = 1;
  # 1212: set OCSP fetch failures (non-stapled, see 1211) to hard-fail
  "security.OCSP.require" = true;
  # 1220: disable or limit SHA-1 certificates
  "security.pki.sha1_enforcement_level" = 1;
  # 1223: enforce strict pinning
  "security.cert_pinning.enforcement_level" = 2;
  # 1270: display warning on the padlock for "broken security" (if 1201 is false)
  "security.ssl.treat_unsafe_negotiation_as_broken" = true;
  # 1271: control "Add Security Exception" dialog on SSL warnings
  "browser.ssl_override_behavior" = 1;
  # 1272: display advanced information on Insecure Connection warning pages
  "browser.xul.error_pages.expert_bad_cert" = true;
  # 1273: display "insecure" icon and "Not Secure" text on HTTP sites
  "security.insecure_connection_icon.enabled" = true;
  "security.insecure_connection_text.enabled" = true;

  # [SECTION 1600]: HEADERS / REFERERS

  # 1603: CROSS ORIGIN: control when to send a referer
  "network.http.referer.XOriginPolicy" = 1;
  # 1610: enable the DNT (Do Not Track) HTTP header
  "privacy.donottrackheader.enabled" = true;

  # [SECTION 1700]: CONTAINERS

  # 1701: enable Container Tabs setting in preferences (see 1702)
  "privacy.userContext.ui.enabled" = true;

  # [SECTION 1800]: PLUGINS

  # 1820: disable GMP (Gecko Media Plugins)
  "media.gmp-provider.enabled" = false;
  # 1825: disable widevine CDM (Content Decryption Module)
  "media.gmp-widevinecdm.visible" = false;
  "media.gmp-widevinecdm.enabled" = false;
  # 1830: disable all DRM content (EME: Encryption Media Extension)
  "media.eme.enabled" = false;

  # [SECTION 2000]: MEDIA / CAMERA / MIC

  # 2002: limit WebRTC IP leaks if using WebRTC
  "media.peerconnection.ice.default_address_only" = true;
  "media.peerconnection.ice.no_host" = true;
  "media.peerconnection.ice.proxy_only_if_behind_proxy" = true;
  # 2030: disable autoplay of HTML5 media
  "media.autoplay.default" = 5;
  # 2031: disable autoplay of HTML5 media if you interacted with the site
  "media.autoplay.enabled.user-gestures-needed" = false;

  # [SECTION 2200]: WINDOW MEDDLING & LEAKS / POPUPS

  # 2201: prevent websites from disabling new window features
  "dom.disable_window_open_feature.close" = true;
  "dom.disable_window_open_feature.location" = true;
  "dom.disable_window_open_feature.menubar" = true;
  "dom.disable_window_open_feature.minimizable" = true;
  "dom.disable_window_open_feature.personalbar" = true;
  "dom.disable_window_open_feature.resizable" = true;
  "dom.disable_window_open_feature.status" = true;
  "dom.disable_window_open_feature.titlebar" = true;
  "dom.disable_window_open_feature.toolbar" = true;
  # 2202: prevent scripts from moving and resizing open windows
  "dom.disable_window_move_resize" = true;
  # 2203: open links targeting new windows in a new tab instead
  "browser.link.open_newwindow" = 3;
  "browser.link.open_newwindow.restriction" = 0;
  # 2210: block popup windows
  "dom.disable_open_during_load" = true;
  # 2212: limit events that can cause a popup
  "dom.popup_allowed_events" = "click dblclick";

  # [SECTION 2300]: WEB WORKERS

  # 2302: disable service workers
  "dom.serviceWorkers.enabled" = false;

  # [SECTION 2400]: DOM (DOCUMENT OBJECT MODEL) & JAVASCRIPT

  # 2402: disable website access to clipboard events/content
  "dom.event.clipboardevents.enabled" = false;
  # 2405: disable "Confirm you want to leave" dialog on page close
  "dom.disable_beforeunload" = true;
  # 2414: disable shaking the screen
  "dom.vibrator.enabled" = false;
  # 2429: enable (limited but sufficient) window.opener protection
  "dom.targetBlankNoOpener.enabled" = true;

  # [SECTION 2600]: MISCELLANEOUS

  # 2601: prevent accessibility services from accessing your browser
  "accessibility.force_disabled" = 1;
  # 2602: disable sending additional analytics to web servers
  "beacon.enabled" = false;
  # 2603: remove temp files opened with an external application
  "browser.helperApps.deleteTempFileOnExit" = true;
  # 2605: block web content in file processes
  "browser.tabs.remote.allowLinkedWebInFileUriProcess" = false;
  # 2606: disable UITour backend so there is no chance that a remote page can use it
  "browser.uitour.enabled" = false;
  "browser.uitour.url" = "";
  # 2611: disable middle mouse click opening links from clipboard
  "middlemouse.contentLoadURL" = false;
  # 2614: limit HTTP redirects (this does not control redirects with HTML meta tags or JS)
  "network.http.redirection-limit" = 10;
  # 2616: remove special permissions for certain mozilla domains
  "permissions.manager.defaultsUrl" = "";
  # 2617: remove webchannel whitelist
  "webchannel.allowObject.urlWhitelist" = "";

  # DOWNLOADS

  # 2650: discourage downloading to desktop
  "browser.download.folderList" = 1;
  # 2651: enforce user interaction for security by always asking where to download
  "browser.download.useDownloadDir" = false;
  # 2652: disable adding downloads to the system's "recent documents" list
  "browser.download.manager.addToRecentDocs" = false;
  # 2653: disable hiding mime types (Options>General>Applications) not associated with a plugin
  "browser.download.hide_plugins_without_extensions" = false;
  # 2654: disable "open with" in download dialog
  "browser.download.forbid_open_with" = true;

  # SECURITY

  # 2680: enforce CSP (Content Security Policy)
  "security.csp.enable" = true;

  # [SECTION 2700]: PERSISTENT STORAGE

  # 2701: disable 3rd-party cookies and site-data
  "network.cookie.cookieBehavior" = 1;
  "browser.contentblocking.category" = "custom";
  # 2702: set third-party cookies (i.e ALL) (if enabled, see 2701) to session-only
  "network.cookie.thirdparty.sessionOnly" = true;
  "network.cookie.thirdparty.nonsecureSessionOnly" = true;

  # [SECTION 4000]: FPI (FIRST PARTY ISOLATION)

  # 4001: enable First Party Isolation
  "privacy.firstparty.isolate" = true;

  # [SECTION 4500]: RFP (RESIST FINGERPRINTING)

  # 4501: enable privacy.resistFingerprinting
  "privacy.resistFingerprinting" = true;
  # 4503: disable mozAddonManager Web API
  "privacy.resistFingerprinting.block_mozAddonManager" = true;
  # 4510: disable showing about:blank as soon as possible during startup
  "browser.startup.blankWindow" = false;
}
