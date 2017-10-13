{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "master": {
            "enabled":          1,
            "hidden":           false,
            "description":      "js",
            "nixexprinput":     "nixtodo",
            "nixexprpath":      "release.nix",
            "checkinterval":    30,
            "schedulingshares": 100,
            "enableemail":      false,
            "emailoverride":    "",
            "keepnr":           3,
            "inputs": {
                "nixpkgs": { "type": "git", "value": "git@github.com:NixOS/nixpkgs-channels.git nixos-17.09-small", "emailresponsible": false },
                "nixtodo": { "type": "git", "value": "git@github.com:basvandijk/nixtodo.git master",                "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
