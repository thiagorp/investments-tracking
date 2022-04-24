{pkgs, ...}: rec
{
  devScript = pkgs.writeShellScriptBin "dev" "make dev";
  testScript = pkgs.writeShellScriptBin "tests" "make test";
  allScripts = [devScript testScript];
}
