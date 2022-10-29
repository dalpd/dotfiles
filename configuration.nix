{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Enable NixOS module for home manager.
      <home-manager/nixos>
    ];

  # Using the latest available version of the kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  # Set your time zone.
  time.timeZone = "Europe/Helsinki";

  # Define your hostname.
  networking.hostName = "paxos";  

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;
  networking.networkmanager.enable = true;

  # Enabled for easyeffects.
  programs.dconf.enable = true;  
  programs.light.enable = true;  
  hardware.bluetooth.enable = true;

  # Pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };  

  users.users.dad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "docker"]; # Enable ‘sudo’ for the user.
  };

  fonts.fonts = with pkgs; [
    font-awesome
    iosevka
    victor-mono
  ];

  home-manager.users.dad = { pkgs, ... }: {    
    home.homeDirectory = "/home/dad";
    home.stateVersion = "22.11";

    services.easyeffects.enable = true;
    
    programs.bash = {
      enable = true;
      historyFile = "/home/dad/.bash_alternative_history";
      historyFileSize = 1000000;
      historySize = 100000;
      historyIgnore = [ "ls" "ll"];
      initExtra =
      ''export PATH=:$PATH:/home/dad/.local/bin:/home/dad/.emacs.d/bin;
      '';
    };
    programs.git = {
      enable = true;
      userName = "dalpd";
      userEmail = "denizalpd@ogr.iu.edu.tr";
      extraConfig = {
        core = { editor = "emacs"; }; 
      };
    };
    programs.emacs.enable = true;
    programs.rofi = {
      enable = true;
      location = "top";
    };

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      };

    programs.home-manager.enable = true;    
    home.packages = (import ./home.nix pkgs).packages;    
  };  

  services = {
    # Enable bluetooth
    blueman.enable = true;
    
    # Enable postgresql, tracking the channel's version
    postgresql.enable = true;
    
    # Enable CUPS
    printing.enable = true;

    # Enable X11
    xserver.enable = true;
    
    # Standalone compositor for Xorg
    picom = {
      enable = true;
      backend = "glx";

      activeOpacity = 0.9;
      inactiveOpacity = 0.75;
      opacityRules = [
        "100:class_g = 'firefox-aurora' && !_NET_WM_STATE@:32a"
        "100:class_g = 'Chromium-browser' && !_NET_WM_STATE@:32a"
        "100:class_g = 'dolphin' && !_NET_WM_STATE@:32a"	
	"100:class_g = 'gwenview' && !_NET_WM_STATE@:32a"
	"100:class_g = 'okular' && !_NET_WM_STATE@:32a"
	"100:class_g = 'TelegramDesktop' && !_NET_WM_STATE@:32a"
	];
      
      fade = true;
      fadeDelta = 15;
      fadeSteps = [ 0.04 0.04 ];
      };
  };

  services.xserver = {
    displayManager.sddm.enable = true;  
    # Enable touchpad support
    libinput.enable = true;      
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      };
    };

  virtualisation.docker.enable = true;
  system.stateVersion = "20.09";
}
