## ADHD'ed DHCP server _(whatever this means)_

[[logo.png]]

ADHD is DHCP server I made for fun. Actually, to use it in all networks I have access to.
Due to nature of DHCP protocol I may intrude any network with some funny leases!

### Main feature
ADHD features a unique "meme-ip" generator. Do you want leet ips? Just add "1337" to list and you'll get possible combinations in offers!

### Build it:
- `nix build`/`cabal build`/`cabal install --installdir=.`
- You can use `./make_release.sh temp` (requires Nix) to build static executable with docs.

### Run it:
- `$EDITOR config.dhall`
- `cabal run`/`nix run`/`./result/bin/adhd`/`./adhd`

### Configuration
Config is provided in dhall file using `-c` argument. Default is `$PWD/config.dhall`
```dhall
{
  network = "13.37.0.0/16", -- Subnet where to distribute addresses
  serverIP = "13.37.22.8", -- IP of the machine where ADHD hosts
  gateway = "13.37.0.1", -- Gateway of the subnet for internet access
  occupiedIPs = ["13.37.67.67", "13.37.13.37"], -- IPs not to distribute (for static ones etc)
                                                -- Gateway and server are ommited automatically
  dns = ["13.37.67.67", "8.8.8.8"], -- Nameservers to include with in DHCP response
  beautifulStrings = ["1337","228","67","999"] -- Strings from which IPs are generated
}
```

### Documentation?
Haddock comments are present in the source. Pre-built docs are distributed in release assets.

### Note about tunnels
If you want to run this server on a device that is routing all traffic through tunnel, you'll need to add a specific route for broadcast:
`ip route add 255.255.255.255/32 dev <your_local_interface>`
