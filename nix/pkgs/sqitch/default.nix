# Based on upstream nixpkgs package:
#
# https://github.com/NixOS/nixpkgs/blob/3557bb49b065495de31b0cbe71de8f2f380c2706/pkgs/development/tools/misc/sqitch/default.nix
#
# But we override the Perl module, which is not well-maintained upstream.

{ stdenv
, lib
, perlPackages
, fetchurl
, makeWrapper
, shortenPerlShebang
, mysqlSupport ? false
, postgresqlSupport ? false
}:

let

  # Generated by running these commands:
  #
  # nix-generate-from-cpan Algorithm::Backoff
  # nix-generate-from-cpan Test::Exit
  # nix-generate-from-cpan Return::MultiLevel
  # nix-generate-from-cpan URI::db
  # nix-generate-from-cpan App::Sqitch

  AlgorithmBackoff = perlPackages.buildPerlPackage {
    pname = "Algorithm-Backoff";
    version = "0.009";
    src = fetchurl {
      url = "mirror://cpan/authors/id/P/PE/PERLANCAR/Algorithm-Backoff-0.009.tar.gz";
      sha256 = "9f0ffcdf1e65a88022d6412f46ad977ede5a7b64be663009d13948fe8c9d180b";
    };
    buildInputs = with perlPackages; [ TestException TestNumberDelta ];
    meta = {
      homepage = "https://metacpan.org/release/Algorithm-Backoff";
      description = "Various backoff strategies for retry";
      license = with lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  ReturnMultiLevel = perlPackages.buildPerlPackage {
    pname = "Return-MultiLevel";
    version = "0.08";
    src = fetchurl {
      url = "mirror://cpan/authors/id/P/PL/PLICEASE/Return-MultiLevel-0.08.tar.gz";
      sha256 = "51b1aef30c5c4009f640267a08589212e87dcd101800f0d20f9c635c9ffe88a1";
    };
    buildInputs = with perlPackages; [ TestFatal ];
    meta = {
      homepage = "https://metacpan.org/pod/Return::MultiLevel";
      description = "Return across multiple call levels";
      license = with lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  TestExit = perlPackages.buildPerlPackage {
    pname = "Test-Exit";
    version = "0.11";
    src = fetchurl {
      url = "mirror://cpan/authors/id/A/AR/ARODLAND/Test-Exit-0.11.tar.gz";
      sha256 = "fbda92d37e0481d18eebc81e48d025228b57184c59b2d5a6f6bdf87042e8c7b2";
    };
    propagatedBuildInputs = [ ReturnMultiLevel ];
    meta = {
      description = "Test whether code exits without terminating testing";
      license = with lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  URIdb = perlPackages.buildPerlModule {
    pname = "URI-db";
    version = "0.20";
    src = fetchurl {
      url = "mirror://cpan/authors/id/D/DW/DWHEELER/URI-db-0.20.tar.gz";
      sha256 = "14c8da15ac209631b8e39fc12451c9b1311ad0b5ca5f90327bdfa978b7d13f14";
    };
    propagatedBuildInputs = with perlPackages; [ URI URINested ];
    meta = {
      homepage = "https://search.cpan.org/dist/URI-db/";
      description = "Database URIs";
      license = with lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  sqitch = perlPackages.buildPerlModule {
    pname = "App-Sqitch";
    version = "1.3.1";
    src = fetchurl {
      url = "mirror://cpan/authors/id/D/DW/DWHEELER/App-Sqitch-v1.3.1.tar.gz";
      sha256 = "sha256-9edo0pjNQEfuKuQjGXgujCzaMSc3vL2/r1gL1H7+i5Q=";
    };
    buildInputs = [
      TestExit
    ] ++ (with perlPackages; [
      CaptureTiny
      ModuleRuntime
      PodParser
      TestDeep
      TestDir
      TestException
      TestFile
      TestFileContents
      TestMockModule
      TestMockObject
      TestNoWarnings
      TestWarn
    ]);
    propagatedBuildInputs =
      [
        AlgorithmBackoff
        URIdb
      ]
      ++ (with perlPackages; [
        Clone
        ConfigGitLike
        DBI
        DateTime
        DateTimeTimeZone
        DevelStackTrace
        EncodeLocale
        HashMerge
        IOPager
        IPCRun3
        IPCSystemSimple
        ListMoreUtils
        Moo
        PathClass
        PerlIOutf8_strict
        StringFormatter
        StringShellQuote
        SubExporter
        TemplateTiny
        Throwable
        TryTiny
        TypeTiny
        URI
        libintl-perl
        namespaceautoclean
      ]);

    # Can't find its home directory during Nix builds.
    doCheck = false;

    meta = {
      homepage = "https://sqitch.org/";
      description = "Sensible database change management";
      license = lib.licenses.mit;
    };
  };

  modules = with perlPackages; [ ]
    ++ lib.optional mysqlSupport DBDmysql
    ++ lib.optional postgresqlSupport DBDPg;
in

stdenv.mkDerivation {
  pname = "sqitch";
  version = sqitch.version;

  nativeBuildInputs = [ makeWrapper ] ++ lib.optional stdenv.isDarwin shortenPerlShebang;

  src = sqitch;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin
    for d in bin/sqitch etc lib share ; do
      # make sure dest alreay exists before symlink
      # this prevents installing a broken link into the path
      if [ -e ${sqitch}/$d ]; then
        ln -s ${sqitch}/$d $out/$d
      fi
    done
  '' + lib.optionalString stdenv.isDarwin ''
    shortenPerlShebang $out/bin/sqitch
  '';
  dontStrip = true;
  postFixup = ''
    wrapProgram $out/bin/sqitch --prefix PERL5LIB : ${perlPackages.makeFullPerlPath modules}
  '';

  meta = {
    inherit (sqitch.meta) description homepage license platforms;
  };
}