{ghc}:
with (import <nixpkgs> {});

let osxLibs = (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);
    depsBase = [zlib llvm_39 git unzip];
    deps = if system == "x86_64-darwin" then depsBase ++ osxLibs else depsBase;
    system = builtins.currentSystem;

in haskell.lib.buildStackProject {
  inherit ghc;
  name = "EPC-tools";
  buildInputs = deps;
}

