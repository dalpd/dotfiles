{ pkgs, ... }:
{  
  packages = with pkgs; [
      alacritty
      chromium
      curl
      fd    
      gcc      
      gimp
      gwenview      
      haskellPackages.ghc
      haskellPackages.zlib
      haskellPackages.postgresql-libpq
      haskellPackages.ghcid
      haskellPackages.cabal-install
      htop
      i3lock
      plasma-pa
      haskellPackages.status-notifier-item      
      dolphin            
      git      
      okular
      nixos-icons
      breeze-icons
      oxygen-icons5
      libsForQt5.oxygen-icons5
      libsForQt5.breeze-icons
      networkmanager
      networkmanager_dmenu
      firefox-devedition-bin
      feh
      scrot
      wget
      python39
      font-awesome
      iosevka
      polybar
      rofi
      unzip
      gnumake
      openssl
      jq
      irssi
      guvcview
      pavucontrol
      mpv
      neofetch
      termonad
      tree
      ripgrep
      tdesktop
      xorg.xmodmap
      zlib
      powertop
    ];
}
