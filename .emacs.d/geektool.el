(defun geektool () 
 (interactive) 
 ;; full-screen frame settings 
 (menu-bar-mode -1) 
 (tool-bar-mode -1) 
 (setq initial-frame-alist '((top . 0) (left . 0) (width . 200) (height . 57))) 
 (setq default-frame-alist '((top . 0) (left . 0) (width . 200) (height . 57))) 
 (setq special-display-frame-alist '((top . 0) (left . 0) (width . 200) (height . 57))) 
 (switch-to-buffer-other-frame (buffer-name)) 
 ;; create windows as needed 
 (delete-other-windows)
 (calendar) 
 (split-window-horizontally) 
 (other-window 2) 
 (split-window) 
 (set-window-text-height nil 25) 
 (split-window-horizontally) 
 (other-window 1) 
 ;; terminal window width 
 (enlarge-window-horizontally 
  (- 80 (window-width))) 
 ;; now start programms 
 (ansi-term "/bin/bash" "top") 
 (insert "top") 
 (term-send-input) 
 (other-window 1) 
 (split-window-horizontally) 
 (switch-to-buffer "ps-top") 
 (enlarge-window-horizontally 
  (- 30 (window-width))) 
 (shell-command "geektool-pstop1.sh" "ps-top") 
 (other-window 1) 
 (split-window) 
 (switch-to-buffer "External IP") 
 (shell-command "echo External IP: `curl -s http://checkip.dyndns.org/ | sed 's/[a-zA-Z/ :]//g'`" "External IP") 
 (split-window) 
 (other-window 1) 
 (switch-to-buffer "Network") 
 (shell-command "my-connections.sh" "Network") 
 (other-window 1) 
 (find-file (file-name-sans-versions "/private/var/log/system.log" t)) 
 (other-window 1) 
 (switch-to-buffer "Uptime") 
 (split-window) 
 (switch-to-buffer "Disk") 
 (other-window 2) 
 (shell-command "uptime | awk '{printf "Uptime: " $3 " " $4 " " $5 " " }'; top -l 1 |
awk '/PhysMem/ {printf "RAM : " $8 ", "}' ; top -l 2 |  
awk '/CPU usage/ && NR > 5 {print $6, $7=":", $8, $9="user", $10, $11="sys", $12, $13}'"
"Uptime") 
 (other-window 1) 
 (shell-command "df -h | grep disk0s3 | awk '{print "Macintosh HD:", $2, "total,", $3, "used,", $4, "remaining"}'" "Disk") 
 (other-window 1) 
 ;; turn off undo for terminal, to avoid memory exhaust 
 (with-current-buffer "*top*" (setq buffer-undo-list t))) 

;(geektool)
