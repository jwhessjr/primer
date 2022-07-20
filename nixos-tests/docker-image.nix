{ testingPython
, primer-service-docker-image
, primer-sqitch
, version
, defaultServicePort
, ...
}:
with testingPython;
let
  database_url = "postgres://primer:foopass@postgres:5432/primer";
  port = toString defaultServicePort;
in
makeTest {
  name = "docker-image";

  meta = with pkgs.lib.maintainers; { maintainers = [ dhess ]; };

  nodes = {
    postgres = { pkgs, config, ... }: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_13;
        enableTCPIP = true;
        authentication = ''
          hostnossl primer primer 192.168.0.0/16 md5
        '';
        initialScript = pkgs.writeText "postgresql-init.sql" ''
          CREATE DATABASE primer TEMPLATE template0 ENCODING UTF8;
          CREATE USER primer WITH PASSWORD 'foopass';
          GRANT ALL PRIVILEGES ON DATABASE primer TO primer;
        '';
      };

      # This is essential, or else Sqitch will fail.
      time.timeZone = "UTC";

      networking.firewall.allowedTCPPorts = [ 5432 ];

      environment.systemPackages = [
        primer-sqitch
      ];
    };

    primer = { pkgs, config, ... }: {
      # This is essential, or else Sqitch will fail.
      time.timeZone = "UTC";

      # Default VM size is too small for our container.
      virtualisation = {
        diskSize = 2048;
        memorySize = 1024;
      };

      virtualisation.oci-containers = {
        containers.primer-service = {
          image = "primer-service:${primer-service-docker-image.imageTag}";
          imageFile = primer-service-docker-image;
          ports = [ "${port}:${port}" ];
          extraOptions = [ "--network=host" ];
          environment = {
            DATABASE_URL = database_url;
          };
        };
      };

      # If the container doesn't start cleanly, the test has failed.
      systemd.services.podman-primer-service.serviceConfig.Restart = pkgs.lib.mkForce "no";

      # Make sure we can see container failures.
      systemd.services.podman-primer-service.serviceConfig.StandardOutput = pkgs.lib.mkForce "journal";
      systemd.services.podman-primer-service.serviceConfig.StandardError = pkgs.lib.mkForce "journal";

      # We want to manually start and stop the container.
      systemd.services.podman-primer-service.wantedBy = pkgs.lib.mkForce [ ];

      environment.systemPackages = [
        pkgs.curl
        pkgs.jq
      ];
    };
  };

  testScript = { nodes, ... }:
    let
      versionCheck = pkgs.writeShellApplication {
        name = "primer-version-check";
        text = ''
          RESULT=$(curl http://localhost:${port}/api/version | jq -r)
          if [ "$RESULT" != "${version}" ]; then
            echo "Expected primer-service version ${version}, but got $RESULT" >& 2
            exit 1
          fi
        '';
      };
    in
    ''
      postgres.start();
      postgres.wait_for_unit("postgresql.service")

      primer.start();
      primer.systemctl("start podman-primer-service.service")
      primer.wait_for_unit("podman-primer-service.service")

      with subtest("fails if the database hasn't been deployed"):
          primer.sleep(5)
          primer.require_unit_state("podman-primer-service.service", "failed")

      postgres.succeed(
          "primer-sqitch deploy --verify db:${database_url}"
      )

      primer.systemctl("start podman-primer-service.service")
      primer.wait_for_unit("podman-primer-service.service")
      primer.wait_for_open_port(${port})

      with subtest("version check"):
          primer.succeed("${versionCheck}/bin/primer-version-check")
    '';
}

