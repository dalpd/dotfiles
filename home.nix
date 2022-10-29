{ pkgs, ... }:
{  
  packages = with pkgs; [
      autorandr
      chromium
      curl
      fd    
      gcc      
      gimp
      gwenview      
      htop
      i3lock
      plasma-pa
      okular
      nixos-icons
      gnome.nautilus
      # libsForQt5.breeze-icons
      # libsForQt5.oxygen-icons5
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
      powertop      
      unzip
      gnumake
      openssl
      jq
      irssi
      guvcview
      pavucontrol
      playerctl
      mpv
      neofetch
      termonad
      tree
      ripgrep
      tdesktop
      xorg.xmodmap
      zlib
    ];
}
