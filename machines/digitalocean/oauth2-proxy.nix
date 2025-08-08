{
  config,
  ...
}:

{
  services.oauth2-proxy = {
    enable = true;
    provider = "oidc";
    scope = "openid profile email groups";
    oidcIssuerUrl = "https://id.kropki.org";
    clientID = "f1f219b2-64b4-4635-b522-dd00990285d3";
    keyFile = config.age.secrets.oauth2-proxy.path or "/secrets/oauth2-proxy";
    reverseProxy = true;
    setXauthrequest = true;
    cookie = {
      domain = "kropki.org";
      refresh = "1h";
      secure = true;
    };
    email.domains = [ "*" ];
    extraConfig = {
      whitelist-domain = ["*.kropki.org"];
      insecure-oidc-allow-unverified-email = true;
      code-challenge-method = "S256";
      insecure-oidc-skip-nonce = false;
    };
  };

  services.nginx.virtualHosts."oauth2.kropki.org" = {
    http3 = true;
    quic = true;
    enableACME = true;
    forceSSL = true;
    kTLS = true;
    locations."/".return = "301 https://oauth2.kropki.org/oauth2/sign_in";
  };

  age.secrets.oauth2-proxy.file = ../../secrets/oauth2-proxy.age;
}
