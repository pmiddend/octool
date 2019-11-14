let
  channelRelease = "nixos-20.03pre200231.7827d3f4497";
  channelName = "unstable";
  url = "https://releases.nixos.org/nixos/${channelName}/${channelRelease}/nixexprs.tar.xz";
  src = builtins.fetchTarball {
    name = "nixos-unstable";
    inherit url;
    sha256 = "080jdwgz176fwwqs09j61yqiic8kvpzg7fxsvqivqgkisw99pvhy";
  };
  pkgs = import src {};
in pkgs.haskellPackages.callPackage ./production.nix {}
