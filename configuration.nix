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

  sound.enable = true;  
  programs.light.enable = true;  
  hardware = {
    pulseaudio.enable = true;
    bluetooth.enable = true;
  };

  users.users.dad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "docker"]; # Enable ‘sudo’ for the user.
  };

  home-manager.users.dad = { pkgs, ... }: {
    
    home.homeDirectory = "/home/dad";
    home.stateVersion = "22.11";

    programs.bash = {
      enable = true;
      historyFile = "/home/dad/.bash_alternative_history";
      historyFileSize = 1000000;
      historySize = 100000;
      historyIgnore = [ "ls" "ll"];
      initExtra = "export PATH=:$PATH:/home/dad/.local/bin:/home/dad/.emacs.d/bin";
    };

    programs.emacs.enable = true;
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
    };

  services.xserver = {
    # Enable touchpad support
    libinput.enable = true;      
    displayManager.sddm.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      };
    };

  virtualisation.docker.enable = true;
  system.stateVersion = "20.09";
}
