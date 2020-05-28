;; (use-package elfeed
;;   (use-package elfeed-goodies)
;;   (elfeed-goodies/setup)
;;
;;   ;; Try http://fetchrss.com/ if the site doesn't have it's own RSS feed.
;;   ;;
;;   ;; (setq elfeed-feeds
;;   ;;       '(
;;   ;;         "http://planet.emacsen.org/atom.xml"))
;;   )

(require 'elfeed)
(require 'elfeed-goodies)

(elfeed-goodies/setup)

(setq elfeed-feeds
      '("https://washingtondc.craigslist.org/search/ctd?auto_bodytype=5&auto_title_status=1&auto_title_status=5&format=rss&max_price=9500&min_auto_year=2012&postal=20164&search_distance=20"))

(provide 'setup-elfeed)
