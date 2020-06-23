{ config, pkgs, ... }:

pkgs.writeTextFile {
  name = "cgitrc";
  text = ''
    # Enable caching of up to 1000 output entries
    cache-size=1000
    
    cache-root=${config.services.nginx.stateDir}/cgit/$HTTP_HOST/
    
    # Show owner on index page
    enable-index-owner=0
    
    # Enable blame page and create links to it from tree page
    enable-blame=1
    
    # Enable ASCII art commit history graph on the log pages
    enable-commit-graph=1
    
    # Show number of affected files per commit on the log pages
    enable-log-filecount=1
    
    # Show number of added/removed lines per commit on the log pages
    enable-log-linecount=1
    
    # Enable cloning via http
    enable-http-clone=1
    clone-url=https://$HTTP_HOST/$CGIT_REPO_URL
    
    # CSS and logo
    css=/cgit.css
    logo=/cgit.png
    
    # Set the title and heading of the repository index page
    root-title=Henri's git repositories
    root-desc=Sources for my programs and notes
    
    # Search for these files in the root of the default branch of repositories
    readme=:README.md
    readme=:readme.md
    readme=:README
    readme=:readme
    
    # disallow crawlers
    robots=noindex, nofollow
    
    # is this needed?
    virtual-root=/
    
    # Format markdown, restructuredtext, manpages, text files, and html files
    # through the right converters
    about-filter=${pkgs.cgit}/lib/cgit/filters/about-formatting.sh
    
    # enable highlighting
    source-filter=${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py
    
    # load info from git
    enable-git-config=1
  '';
}
