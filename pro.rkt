#lang racket
(require net/url)
(require (planet neil/html-parsing:3:0))
(define (helper s l c)
  (cond ((member s (flatten (car l))) c)
        (else (helper s (cdr l) (+ 1 c)))))
(define (getpos s l m)
  (cond ((not (list? (list-ref l (helper s l 0)))) (reverse (cons (helper s l 0) m)))
        (else (getpos s (list-ref l (helper s l 0)) (cons (helper s l 0) m)))))
;(define k (html->xexp (port->string (get-pure-port
 ;    (string->url "http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22st-match-indian-premier-league-2018")))))
(define k 
  '(*TOP*
    "\r\n"
    "\r\n"
  (*DECL* DOCTYPE html)
  (html
   (@
    (lang "en")
    (itemscope)
    (itemtype "http://schema.org/WebPage"))
   (head
    (meta (@ (charset "utf-8")))
    (script
     "var is_mobile = /symbian|tizen|midp|uc(web|browser)|MSIE (5.0|6.0|7.0|8.0)|tablet/i.test(navigator.userAgent);\tif(is_mobile && window.location.hostname != \"www1.cricbuzz.com\") window.location.hostname = \"m.cricbuzz.com\";")
    (style
     "body{background:#E3E6E3; font-family: helvetica,\"Segoe UI\",Arial,sans-serif;color:#222;font-size:14px; line-height: 1.5; margin:0;}\tbody, .cb-comm-pg, .cb-hm-mid {min-height:1000px}\t.container{width:980px;margin:0 auto;}\t.page{max-width: 980px;margin: 0 auto;position: relative;}\t.cb-col-8 {width:8%;}\t.cb-col-10 {width:10%;}\t.cb-col-14 {width:14%;}\t.cb-col-16 {width:16%;}\t.cb-col-20 {width:20%;}\t.cb-col-25 {width:25%;}\t.cb-col-27 {width:27%;}\t.cb-col-33 {width:33%;}\t.cb-col-40 {width:40%;}\t.cb-col-46 {width:46%;}\t.cb-col-47 {width:47%;}\t.cb-col-50 {width:50%;}\t.cb-col-60 {width:60%;}\t.cb-col-66 {width:66%;}\t.cb-col-67 {width:67%;}\t.cb-col-73 {width:73%;}\t.cb-col-75 {width:75%;}\t.cb-col-84 {width:84%;}\t.cb-col-80 {width:80%;}\t.cb-col-90{width:90%;}\t.cb-col-100 {width:100%;}\t.cb-col {display: inline-block;box-sizing: border-box;float: left; min-height: 1px;}\th1 {font-size: 36px;line-height: 42px; margin:0;}\th2 {font-size: 24px; margin:0; line-height: 30px;}\th3 {font-size: 18px;line-height: 24px; margin:0; }\th4 {font-size: 16px; margin:0; }\th5 {font-size: 14px; margin: 0;}\t.cb-font-18 {font-size:18px;}\timg{border: 0;}\ta {text-decoration: none; color:#222;}\ta, a:hover, a:active, a:focus {outline: medium none;}\t.text-center {text-align: center;}\t.cb-nws-lft-col{padding:15px 20px;}\t.cb-nws-dtl-lft-col {padding: 10px 30px 0 30px;border-right: 1px solid #ecebeb ;}\t.cb-nws-lst-rt{padding-left: 10px;}\t.cb-srs-lst-itm {padding: 10px 0;}\t.cb-lst-itm-sm{padding:10px 0 5px;}\t.cb-scrd-lft-col{padding:15px 10px;} .cb-col-rt{padding: 10px;}\t.text-white{color: #fff;}\t.cb-scrd-hdr-rw, .cb-nav-pill-1.active {background: #028062 ;color: #fff;}\t.cb-nav{position:relative; height:48px;background: #009270;}\t.cb-hm-mnu-itm{padding: 16px 8px 11px; color:#fff; display:inline-block;}\t.cb-hm-text{padding:10px 20px;}\t.cb-hm-rght{padding: 15px;}\t.cb-subnav .cb-sub-navigation {display: none; position: absolute;}\t.cb-mat-mnu{background:#4a4a4a; width:980px; font-size: 0;}\t.cb-mat-mnu-itm{font-size:12px; color:#fff; padding:10px; cursor: pointer; display:inline-block; max-width: 140px;}\t.cb-mat-mnu-ttl{background: #333; padding: 10px 20px;}\t.cb-mat-mnu-wrp{margin-bottom: 10px;}\t.cb-ovr-flo{overflow: hidden; white-space: nowrap; text-overflow: ellipsis;}\t.cb-mat-mnu-all{text-align: center; float: right; width:60px;}\t.cb-nav-main{border-bottom:1px solid #e3e6e3;padding:0 0 5px 20px;}\t.cb-nav-bar{padding-top:10px;clear:both;}\t.cb-lst-itm, .cb-bg-white{background: #fff;}\t.cb-nav-tab.active, .cb-nav-tab-hm.active{ font-weight: bold; color: #028062;}\t.cb-nav-tab.active {border-bottom: 3px solid #028062; line-height: 24px; }\t.cb-nav-tab-hm.active{ border-bottom:2px solid #028062; line-height: 22px;}\t.cb-nav-tab{margin-right:20px;padding-bottom:6px;}\t.cb-nav-tab-hm{margin-right:15px;padding-bottom:6px;}\t.cb-nav-hdr{padding-top:15px}\t.cb-nav-subhdr{padding:5px 0 10px 0;}\t.cb-lv-scr-mtch-hdr{line-height: 21px;font-size: 16px;}\t.disp-none{display:none;}\t.disp-blck{display:block;}\t.cb-font-24{font-size: 24px;}\t.line-ht30{line-height: 30px;}\t.line-ht24{line-height: 24px;}\t.cb-hm-scg-blk{background: #fff; margin-bottom: 5px; height:90px;}\t.mrgn-btm-5{margin-bottom : 5px;}\t#scagTabContent .show{display: block; visibility: visible;}\t#scagTabContent .hide{display: none; visibility: hidden;}\t[ng\\:cloak], [ng-cloak], [data-ng-cloak], [x-ng-cloak], .ng-cloak, .x-ng-cloak { display: none !important;}\t.cb-hm-lft{margin-bottom:5px;padding:0 15px;}\t.cb-hm-lft-hdr{margin:0; padding:15px 15px 0; color: #009270;} .cb-hm-mid {border-left: 5px solid #e3e6e3 ;border-right: 5px solid #e3e6e3 ;padding: 0 15px;} .cb-mtch-blk {border-right: 1px solid #ecebeb ;line-height: 1.4;margin: 15px 0;padding: 0 20px;}\t.crd-cntxt{font-size: 12px; color:#666; padding-bottom: 10px;}\t.big-crd-main{border-top: 1px solid #ecebeb;padding:15px 0 5px; margin-bottom: 1px;clear: both;} .big-crd-reltd-itm {margin: 0 0 10px; width:100%; display: inline-block;}\t.cb-nws-time{font-size:12px; padding-bottom: 5px;}\t.cb-nws-hdln-ancr{padding-bottom: 5px;}\t.cb-hmscg-bwl-txt, .cb-hmscg-bat-txt{font-weight:bold; padding-bottom:4px; height:18px;}\t.cb-hmscg-bwl-txt{color:#666;}\t.sml-crd-main{padding: 15px 0 5px; width: 100%;}\t.big-crd-hdln{margin: 10px 0;}\t.big-crd-rltd-txt {font-weight: bold; margin:10px 0; color:#1866DB;} .cb-nws-intr {color: #666;padding-bottom: 5px;} .cb-lv-scrs-well {background: #f5f5f5 ;display: inline-block;margin: 10px 0;padding: 10px 5px;} .cb-schdl-hdr {margin: 0;padding: 15px 0 10px 10px;} .cb-lst-itm-lg {padding: 20px 0;} .cb-left-pad {padding: 20px 10px 0;}\t.cb-nav-pill-1 {background: #CFE0DB;border-radius: 20px;color: #222;cursor: pointer;margin-right: 15px;padding: 5px 20px;} .cb-mtch-lst {padding: 15px 0;}\t.cb-nws-min-cntr{padding: 10px; background: #f5f5f5; color:#333; margin:10px 0 15px;}\t.sml-crd-subtxt{margin:0 10px 10px;}\t/*-------------------------Live Pages -----------------------*/\t.cb-scrcrd-status{padding:0 0 10px 10px;}\t.cb-scrd-hdr-rw{padding:8px 10px;}\t.cb-bg-gray{background: #ecebeb;}\t.cb-lv-grn-strip{background: #ecebeb;}\t.cb-min-hdr-rw, .cb-scrd-sub-hdr{padding: 4px 10px; font-size: 12px; color: #666;}\t.cb-scrd-itms{padding: 4px 10px; font-size: 13px;}\t.cb-text-link {color: #1866DB;}\t.text-bold{font-weight: bold;}\t.text-normal{font-weight: normal;}\t.cb-mat-fct-itm{padding: 5px 0;}\t.cb-min-bat-rw{padding-bottom: 5px;}\t.cb-nws-sub-txt, .cb-nws-sub-txt, .cb-font-12{font-size:12px;}\t.cb-font-16{font-size:16px;}\t.cb-font-20{font-size:20px;}\t.cb-min-inf{padding-top:10px; margin-left: -10px; display: inline-block;}\t.cb-min-itm-rw{padding:5px 10px 0;}\t.text-right{text-align:right;}\t.cb-key-st-lst{padding: 10px 0 0 5px; }\t.cb-min-prw-time{padding:20px 0 0;}\t.cb-toss-sts{padding: 10px 0 30px;}\t.cb-min-rcnt{padding: 7px 10px; margin: 0 -10px 0;}\t.cb-key-lst-wrp{border: 1px solid #ecebeb; padding-bottom: 15px; margin-right: -10px;}\t.cb-min-tm{font-size: 18px; font-weight: bold;}\t.cb-min-stts{padding: 20px 0;}\t.cb-mom-itm{padding: 0 0 10px;}\t.cb-lv-grn-strip {padding: 10px 10px 5px;}\t.cb-ovr-num{padding-top:2px;}\t.cb-min-pad{padding-left: 5px;}\t/*Upcoming Series*/\t.cb-mnth{margin-top: 10px; padding: 0 10px;}\t.cb-sch-lst-itm {padding:10px 0; margin-left: 20px;border-bottom: 1px solid #ecebeb;}\t/*Matches By Day*/\t.cb-mtchs-dy{padding:20px 20px 20px 10px ;}\t.cb-mtchs-dy-tm,.cb-mtchs-dy-vnu{padding:20px 20px 20px 0px;}\t/*Schedule Teams*/\t.cb-lv-upcom-strip{padding: 5px 10px;background: #ecebeb;}\t/*Archives*/\t.cb-srs-cat{padding:10px; color:#028062; }\t.pad-left{padding-left:30px;}\t.cb-arcv-yr{font-size:20px; padding: 0 0 5px 10px;font-weight: bold}\t.cb-yr-tmline{padding: 2px 15px 15px 0;}\t.cb-sch-tms-widgt{padding: 5px 10px; margin:5px 2px 2px 2px; background: #f5f5f5; border-radius: 0; border:0; display: inline-block;}\t/*Photos*/\t.cb-thmb-dark {background: #333333; box-shadow:none; border-radius:0; border:none; padding:0;color:#CCC;}\t.cb-pht-main {padding:20px 15px;margin: 0 -10px;}\t.cb-pht-block{padding:9px;float:left;height:280px;box-sizing: border-box;} .cb-gallery-pht-block{padding: 9px 9px 9px 16px;}\t.img-responsive{height:auto; max-width:100%;}\t.center-block{margin-right: auto;margin-left: auto;}\t.cb-schdl{padding:0 10px;line-height: 1.5;}\t.cb-caret-up, .cb-caret-down{display: inline-block; width: 0; height: 0; margin-left: 4px; margin-bottom: 1px; border-right: 4px solid transparent; border-left: 4px solid transparent;}\t.cb-caret-up{border-bottom: 4px solid;}\t.cb-caret-down{border-top: 4px solid;}\t.cb-hm-rt-itm{margin: 0 0 5px; padding: 10px 10px 5px;}\t.cb-hmscg-tm-nm{display:inline-block; width: 60px;}\t.pull-right,.cb-all-mtch-tab{float:right;}\t.cb-skin-ads-close{display: none;}\t.cb-nws-sub-txt{padding-top:10px;}\t.nws-dtl-hdln{margin-top:10px;}\t.cb-min-lv{min-height:270px}\t.cb-min-comp{min-height:150px;}\t.cb-ttl-vts{margin-top:20px;}\t.cb-poll-radio{width: 5%;margin:4px 10px 0 0;}\t.cb-mini-tim{padding-bottom:20px;}\t.cb-com-ln{margin: 0 0 10px; line-height: 24px;}\t.cb-comm-static{min-height:31px; margin: 0 -10px 10px;}\t.cb-com-ovr-sum-ad{min-height: 31px;}\t.cb-comm-static-anchr{margin:5px 10px; display: block;}\t.ad-unit-rendered{margin-bottom: 5px;}\t.cb-mm-wrp { max-height: 0px; -webkit-transition: max-height 0.35s ease; transition: max-height 0.35s ease; overflow: hidden; }\t.cb-mm-wrp.down { max-height: 1000px; transition: max-height 0.75s ease; -webkit-transition: max-height 0.75s ease; overflow: hidden;} .cb-srs-hstry-dtl {padding:10px 15px;margin-top:20px; border-radius: 4px;}\t.cb-qck-lnk{margin-bottom: 5px; padding:10px 15px;}\t.cb-qck-hdr{padding-right: 15px; border-right: 1px solid #ecebeb ;}\t.cb-qck-ancr{margin-left: 15px;}\t.cb-lst-vid-rw{padding-bottom: 0; height: 64px; border: 1px solid #ecebeb; margin-right: -10px;}\t.cb-auth-img{border-radius: 100%;}\t.cb-expt-athr{vertical-align: top; padding: 5px 0 0 0; display: inline-block; font-size: 16px}\t.inline-block{display: inline-block;}\t.cb-exprt-athr-hdr{text-align: right;font-size: 42px;font-family: bodani; color: #fff;padding-right: 15px;line-height:44px;text-transform:uppercase;}\t.cb-exprt-athr-hdr-tag{text-align: right;font-size: 24px;font-family: bodani; color: #fff;padding-right: 15px;font-style:italic;}\t.cb-athr-wgt-wrp{border:1px solid #ecebeb; padding:15px; margin-bottom:20px;background:#f5f5f5;}\t.cb-exprt-athr-hdr-img{background: url(\"//i.cricketcb.com/statics/site/images/harsha-banner.jpg\") no-repeat scroll; height:80px;}\t.cb-overflow-hidden{overflow: hidden;}\t/*Videos*/\t.cb-vid-sm-ply-api {color: #fff;line-height: 34px;font-size: 18px;margin-left: 3px;}\t.cb-vid-sml-card-api {margin-top: 10px;height: auto;padding: 0 12px 0 11px;}\t.cb-cat-head-wrap {padding: 0 12px 0 11px;}\t.cb-cat-head-text {margin-top: 5px;line-height: 20px;}\t.cb-more-btn {padding: 8px 20px;border-radius: 2px;color: #fff;background: #009270;border: 0;cursor: pointer;}\t.cb-cat-head-link {float: right;padding: 5px 25px;margin-top: 10px;border-radius: 4px;}\t.cb-pos-rel{position: relative;}\t.cb-videos-cat {border-bottom: 1px solid #ecebeb;padding: 10px 3px 15px 4px;}\t.cb-cat-head-text-wrap {float: left;}\t.cb-cat-head-count {margin: 0;color: #666;}\th2.cb-cat-head-text{font-size : 18px}\t.cb-vid-sml-card-api-head {font-size: 14px;font-weight: bold;line-height: 18px;max-height: 55px;margin: 2px 0 5px;overflow: hidden;}\t.cb-cen {position: absolute;top: 50%;left: 50%;transform: translateX(-50%) translateY(-50%);-webkit-transform: translateX(-50%) translateY(-50%);-moz-transform: translateX(-50%) translateY(-50%);-ms-transform: translateX(-50%) translateY(-50%);-o-transform: translateX(-50%) translateY(-50%);background: #222;text-align: center;height: 35px;width: 35px;border-radius: 50px;}\t.padding-handling-errors{padding: 20px 0px 10px 15px;border-bottom: 1px solid #ecebeb;}\t.cb-col .cb-align-cen{float: none; text-align: center; padding: 7px 25px; margin-top: 20px;}\t.cb-align-cen{padding-top: 10px;}")
    (script
     "function getCookie(c_name){\tvar i,x,y,ARRcookies=document.cookie.split(\";\");\tfor (i=0;i<ARRcookies.length;i++){\tx=ARRcookies[i].substr(0,ARRcookies[i].indexOf(\"=\"));\ty=ARRcookies[i].substr(ARRcookies[i].indexOf(\"=\")+1);\tx=x.replace(/^\\s+|\\s+$/g,\"\");\tif (x==c_name){\treturn unescape(y);\t}\t}\t}\tvar cbads_value = getCookie(\"cbzads\");\tif(cbads_value != null && cbads_value.indexOf('IN|AS') >= 0){\tdocument.write(\"<style>.cb-geo-in-50{height:50px}</style>\")\t}")
    (meta
     (@ (http-equiv "X-UA-Compatible") (content "IE=edge")))
    (meta (@ (name "author") (content "")))
    (link (@
           (rel "shortcut icon")
           (type "image/x-icon")
           (href
            "//i.cricketcb.com/statics/site/images/cric_ball.ico")))
    (meta
     (@
      (name "google-signin-client_id")
      (content
       "125255339112-d594jen12o0j854sufu8jfn2gbbnaskj.apps.googleusercontent.com")))
    (link (@
           (rel "apple-touch-icon")
           (href
            "//i.cricketcb.com/statics/site/images/apple-touch-icon.png")))
    (link (@
           (rel "chrome-webstore-item")
           (href
            "https://chrome.google.com/webstore/detail/cricbuzz/opljecakjchbhhikbeifamamnpcdbgem")))
    " "
    (title
     (@ (itemprop "name"))
     "Delhi Daredevils vs Kings XI Punjab, 22nd Match, Indian Premier League, 2018 | Cricbuzz.com")
    (meta
     (@
      (name "description")
      (itemprop "description")
      (content
       "Follow Delhi Daredevils vs Kings XI Punjab, 22nd Match, Apr 23, Indian Premier League, 2018 with live Cricket score, ball by ball commentary updates on Cricbuzz")))
    (link (@
           (rel "alternate")
           (href
            "android-app://com.cricbuzz.android/http/www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")))
    (link (@
           (rel "alternate")
           (href
            "//m.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (media "only screen and (max-width: 640px)")))
    (meta (@ (name "robots") (content "index, follow")))
    (meta (@ (name "googlebot") (content "index, follow")))
    (meta
     (@
      (name "google-site-verification")
      (content "google89fd37b1258ef4b9.html")))
    (meta
     (@
      (name "p:domain_verify")
      (content "fd560568b43f31382de30f628c8430bd")))
    (meta
     (@
      (name "msvalidate.01")
      (content "A509FA8BAE50018F9DF2553AEDEDF77B")))
    " "
    (meta
     (@
      (property "og:title")
      (content
       "Delhi Daredevils vs Kings XI Punjab, 22nd Match, Indian Premier League, 2018")))
    (meta
     (@
      (property "og:description")
      (content
       "Follow Delhi Daredevils vs Kings XI Punjab, 22nd Match, Apr 23, Indian Premier League, 2018 with live Cricket score, ball by ball commentary updates on Cricbuzz")))
    (meta (@ (property "fb:app_id") (content "30119633160")))
    (meta (@ (property "og:site_name") (content "Cricbuzz")))
    " "
    (meta
     (@
      (property "twitter:title")
      (content
       "Delhi Daredevils vs Kings XI Punjab, 22nd Match, Indian Premier League, 2018")))
    (meta
     (@
      (property "twitter:description")
      (content
       "Follow Delhi Daredevils vs Kings XI Punjab, 22nd Match, Apr 23, Indian Premier League, 2018 with live Cricket score, ball by ball commentary updates on Cricbuzz")))
    (meta (@ (name "twitter:site") (content "@cricbuzz")))
    (meta (@ (name "twitter:domain") (content "cricbuzz.com")))
    (meta
     (@ (name "twitter:app:name:iphone") (content "Cricbuzz")))
    (meta
     (@ (name "twitter:app:id:iphone") (content "360466413")))
    (meta
     (@
      (name "twitter:app:name:googleplay")
      (content "Cricbuzz")))
    (meta
     (@
      (name "twitter:app:id:googleplay")
      (content "com.cricbuzz.android")))
    (meta (@ (name "twitter:widgets:csp") (content "on")))
    (meta (@ (property "fb:pages") (content "178697151159")))
    (meta (@ (name "keywords") (content "")))
    (meta (@ (name "news_keywords") (content "")))
    (link (@
           (rel "canonical")
           (href
            "http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")))
    (meta (@ (name "msapplication-config") (content "none")))
    (script
     "var PAGE_NAME = \"commentary-page\";\tvar CBQueue = CBQueue || [];\tvar CBQueueOnLoad = CBQueueOnLoad || [];\t/*CBQueue.push(function(){roadblock(2,{country:[\"US\",\"CA\", \"ZA\", \"AU\"], continent:[\"EU\"]});});*/\tCBQueue.push(function(){roadblock(2,{country:[\"US\"]});});\t_udn = \"cricbuzz.com\";")
    (script
     "(function(w){\t\"use strict\";\tvar loadCSS = function( href, before, media ){\tvar doc = w.document;\tvar ss = doc.createElement( \"link\" );\tvar ref;\tif( before ){\tref = before;\t}\telse {\tvar refs = ( doc.body || doc.getElementsByTagName( \"head\" )[ 0 ] ).childNodes;\tref = refs[ refs.length - 1];\t}\tvar sheets = doc.styleSheets;\tss.rel = \"stylesheet\";\tss.href = href;\tss.media = \"only x\";\tref.parentNode.insertBefore( ss, ( before ? ref : ref.nextSibling ) );\tvar onloadcssdefined = function( cb ){\tvar resolvedHref = ss.href;\tvar i = sheets.length;\twhile( i-- ){\tif( sheets[ i ].href === resolvedHref ){\treturn cb();\t}\t}\tsetTimeout(function() {\tonloadcssdefined( cb );\t});\t};\tss.onloadcssdefined = onloadcssdefined;\tonloadcssdefined(function() {\tss.media = media || \"all\";\t});\treturn ss;\t};\tif( typeof module !== \"undefined\" ){\tmodule.exports = loadCSS;\t}\telse {\tw.loadCSS = loadCSS;\t}\t}( typeof global !== \"undefined\" ? global : this ));\tloadCSS( \"//s.cricketcb.com/site/style.201804091043.css\" );")
    (noscript
     (link (@
            (rel "stylesheet")
            (type "text/css")
            (href
             "//s.cricketcb.com/site/style.201804091043.css")))))
   (body
    (a
     (@
      (href "https://plus.google.com/104502282508811467249")
      (rel "publisher")))
    (a
     (@
      (id "ad-skin")
      (target "_blank")
      (href "Javascript:void(0)")
      (class "cb-skin-ads-link cb-skin-ads-link-fixed ad-skin")))
    (a
     (@
      (href "Javascript:void(0)")
      (class "cb-skin-ads-close cb-font-18 ad-skin-close")
      (style
       "position:fixed; z-index:1001; color:#FFF; background:#000; padding:2px 5px; right:2px;"))
     "✖")
    (header
     (@
      (id "top")
      (style
       "z-index: 1000;position: relative;padding-top:10px;width:980px;margin:0 auto;")
      (itemscope)
      (itemtype "http://schema.org/WPHeader"))
     (div
      (@ (class "container"))
      (div
       (@
        (id "leaderboard")
        (class "ad-unit text-center center-block")
        (style "min-height:90px;margin-bottom:10px;")))
      (div
       (@
        (id "countdown")
        (class "ad-unit")
        (style
         "margin:-10px 15px 0px;float:right;position:absolute;")))
      (div (@ (class "toi-branding toi-referral")))
      (nav
       (@
        (class "cb-nav cb-col cb-col-100")
        (id "cb-main-menu"))
       (a
        (@ (href "/") (target "_self") (class "cb-hm-text"))
        (img
         (@
          (itemprop "image")
          (height "30")
          (width "101")
          (style "bottom: -4px; position: relative;")
          (alt "Cricbuzz Logo")
          (title "Cricbuzz Logo")
          (src
           "//i.cricketcb.com/statics/site/images/cbz-logo.png"))))
       (a
        (@
         (class "cb-hm-mnu-itm")
         (target "_self")
         (href "/cricket-match/live-scores")
         (title "Live Cricket Score"))
        "Live Scores")
       (a
        (@
         (class "cb-hm-mnu-itm")
         (target "_self")
         (href "/cricket-schedule/series")
         (title "Cricket Schedule"))
        "Schedule")
       (a
        (@
         (class "cb-hm-mnu-itm")
         (target "_self")
         (href "/cricket-scorecard-archives")
         (title "Cricket Scorecard Archives"))
        "Archives")
       (div
        (@
         (class "cb-subnav cb-hm-mnu-itm feature-button cursor-pointer")
         (id "newsDropDown"))
        (a
         (@
          (class "text-white")
          (target "_self")
          (href "/cricket-news"))
         "News")
        (span (@ (class "cb-caret-down")))
        (nav
         (@ (class "cb-sub-navigation"))
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news")
           (title "Latest Cricket News"))
          "All Stories")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/latest-news")
           (title "Latest Cricket News"))
          "Latest News")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/info/")
           (title "Latest Cricket Topics"))
          "Topics")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/spotlight")
           (title "Cricket Editorials and Specials"))
          "Spotlight")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/editorial-list")
           (title "Latest Cricket Opinions & Editorials"))
          "Opinions")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/specials")
           (title "Latest Cricket Specials"))
          "Specials")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/stats-analysis")
           (title "Latest Cricket Stats & Analysis"))
          "Stats & Analysis")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/interviews")
           (title "Latest Cricket Player Interviews"))
          "Interviews")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/editorial/live-blogs")
           (title "Latest Cricket Match live blogs"))
          "Live Blogs")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/cricket-news/experts/harsha-bhogle/170")
           (title "Articles and Videos by Harsha Bhogle"))
          "Harsha Bhogle")))
       (div
        (@
         (class "cb-subnav cb-hm-mnu-itm feature-button cursor-pointer")
         (id "seriesDropDown"))
        (a
         (@
          (class "text-white")
          (target "_self")
          (href "/cricket-series"))
         "Series")
        (span (@ (class "cb-caret-down")))
        (nav
         (@ (class "cb-sub-navigation"))
         (a
          (@
           (class "cb-subnav-item")
           (href
            "/cricket-series/2676/indian-premier-league-2018")
           (title "Indian Premier League, 2018"))
          "Indian Premier League, 2018")
         (a
          (@
           (class "cb-subnav-item")
           (href
            "/cricket-series/2659/county-championship-division-one-2018")
           (title "County Championship Division One, 2018"))
          "County Championship Division One, 2018")
         (a
          (@
           (class "cb-subnav-item")
           (href
            "/cricket-series/2660/county-championship-division-two-2018")
           (title "County Championship Division Two, 2018"))
          "County Championship Division Two, 2018")
         (a
          (@
           (class "cb-subnav-item")
           (href
            "/cricket-series/2684/super-four-provincial-tournament-2018")
           (title "Super Four Provincial Tournament, 2018"))
          "Super Four Provincial Tournament, 2018")))
       (div
        (@
         (class "cb-subnav cb-hm-mnu-itm feature-button cursor-pointer")
         (id "teamDropDown"))
        (a
         (@
          (class "text-white")
          (target "_self")
          (href "/cricket-team"))
         "Teams")
        (span (@ (class "cb-caret-down")))
        (nav
         (@ (class "cb-sub-navigation cb-sub-lg"))
         " "
         (div
          (@ (class "cb-sub-lg-outer"))
          " "
          (div
           (@ (class "cb-sub-lg-sec"))
           " "
           (h4 (@ (class "cb-sub-lg-sec-head")) "Test Teams")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/india/2")
             (title "India Cricket Team"))
            "India")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/pakistan/3")
             (title "Pakistan Cricket Team"))
            "Pakistan")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/australia/4")
             (title "Australia Cricket Team"))
            "Australia")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/sri-lanka/5")
             (title "Sri Lanka Cricket Team"))
            "Sri Lanka")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/bangladesh/6")
             (title "Bangladesh Cricket Team"))
            "Bangladesh")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/england/9")
             (title "England Cricket Team"))
            "England")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/windies/10")
             (title "Windies Cricket Team"))
            "Windies")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/south-africa/11")
             (title "South Africa Cricket Team"))
            "South Africa")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/zimbabwe/12")
             (title "Zimbabwe Cricket Team"))
            "Zimbabwe")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/new-zealand/13")
             (title "New Zealand Cricket Team"))
            "New Zealand")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/ireland/27")
             (title "Ireland Cricket Team"))
            "Ireland")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/afghanistan/96")
             (title "Afghanistan Cricket Team"))
            "Afghanistan")
           " ")
          " "
          (div
           (@ (class "cb-sub-lg-sec"))
           " "
           (h4 (@ (class "cb-sub-lg-sec-head")) "Associate")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/united-arab-emirates/7")
             (title "United Arab Emirates Cricket Team"))
            "United Arab Emirates")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/hong-kong/8")
             (title "Hong Kong Cricket Team"))
            "Hong Kong")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/kenya/14")
             (title "Kenya Cricket Team"))
            "Kenya")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/united-states-of-america/15")
             (title "United States of America Cricket Team"))
            "United States of America")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/scotland/23")
             (title "Scotland Cricket Team"))
            "Scotland")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/netherlands/24")
             (title "Netherlands Cricket Team"))
            "Netherlands")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/bermuda/25")
             (title "Bermuda Cricket Team"))
            "Bermuda")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/canada/26")
             (title "Canada Cricket Team"))
            "Canada")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/uganda/44")
             (title "Uganda Cricket Team"))
            "Uganda")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/malaysia/71")
             (title "Malaysia Cricket Team"))
            "Malaysia")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/nepal/72")
             (title "Nepal Cricket Team"))
            "Nepal")
           " "
           (a
            (@
             (class "cb-subnav-item cb-sub-lg-sec-item")
             (href "/cricket-team/germany/77")
             (title "Germany Cricket Team"))
            "Germany")
           " ")
          " ")
         " "
         (a
          (@
           (target "_self")
           (href "/cricket-team")
           (class "cb-sub-lg-more"))
          "More...")))
       (div
        (@
         (class "cb-subnav cb-hm-mnu-itm feature-button cursor-pointer")
         (id "videosDropDown"))
        (a
         (@
          (class "text-white")
          (target "_self")
          (href "/cricket-videos"))
         "Videos")
        (span (@ (class "cb-caret-down")))
        (nav
         (@ (class "cb-sub-navigation"))
         (a
          (@
           (class "cb-subnav-item")
           (href "/cricket-videos")
           (title "All Cricket Videos"))
          "All Videos")
         (a
          (@
           (class "cb-subnav-item")
           (href "/cricket-videos/categories")
           (title "Cricket Videos Categories"))
          "Categories")
         (a
          (@
           (class "cb-subnav-item")
           (href "/cricket-videos/playlists")
           (title "Cricket Videos Playlists"))
          "Playlists")))
       (a
        (@
         (class "cb-hm-mnu-itm")
         (target "_self")
         (href "/cricket-photo-gallery")
         (title "Photo Gallery"))
        "Photos")
       (a
        (@
         (class "cb-hm-mnu-itm")
         (target "_self")
         (href "/cricket-stats/icc-rankings")
         (title "ICC Rankings"))
        "Rankings")
       (div
        (@
         (class "cb-subnav cb-hm-mnu-itm feature-button cursor-pointer"))
        "More"
        (span (@ (class "cb-caret-down")))
        (nav
         (@ (class "cb-sub-navigation") (style "right:0px;"))
         " "
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/mobileapps")
           (title "Mobile Apps"))
          "Mobile Apps")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/careers")
           (title "Careers"))
          "Careers")
         (a
          (@
           (class "cb-subnav-item")
           (target "_self")
           (href "/info/contact")
           (title "Contact Us"))
          "Contact Us")))
       " ")))
    (div
     (@ (class "page"))
     (div (@ (id "fb-root")))
     (div
      (@
       (id "page-wrapper")
       (class "container")
       (style "display:inline-block;"))
      (div
       (@
        (id "shosh")
        (class "ad-unit shosh-embed")
        (style "height:0; width:980px; text-align:center;")))
      (span
       (@
        (id "skin_left")
        (class "ad-unit")
        (style
         "overflow:hidden; position:fixed;top:0;left:calc(50% - 635px);margin-right:3px;z-index:-99;")))
      " "
      (div
       (@
        (class "html-refresh")
        (url "/api/html/matches-menu")
        (timeout "300000")
        (disable-first-load "true"))
       (div
        (@ (class "cb-col cb-col-100 mrgn-btm-5"))
        (nav
         (@ (class "cb-mat-mnu") (ng-init "direction='up'"))
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo cb-mat-mnu-ttl")
           (title "")
           (href "/cricket-match/live-scores")
           (id "live-scores-link"))
          "MATCHES")
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo")
           (title
            "Delhi Daredevils v Kings XI Punjab,22nd Match - Live")
           (href
            "/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (target "_self"))
          "DD vs KXIP - Live")
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo")
           (title
            "Mumbai Indians v Sunrisers Hyderabad,23rd Match - Preview")
           (href
            "/live-cricket-scores/20083/mi-vs-srh-23rd-match-indian-premier-league-2018")
           (target "_self"))
          "MI vs SRH - Preview")
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo")
           (title "Dambulla v Colombo,5th Match - Stumps")
           (href
            "/live-cricket-scores/20155/dambulla-vs-colombo-5th-match-super-four-provincial-tournament-2018")
           (target "_self"))
          "DAMBULLA vs COLOMBO - Stumps")
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo")
           (title "Galle v Kandy,6th Match - Stumps")
           (href
            "/live-cricket-scores/20156/galle-vs-kandy-6th-match-super-four-provincial-tournament-2018")
           (target "_self"))
          "GALLE vs KANDY - Stumps")
         (a
          (@
           (class "cb-mat-mnu-itm cb-ovr-flo")
           (title
            "Gloucestershire v Glamorgan,County Div 2 - GLAM Won")
           (href
            "/live-cricket-scores/19465/gloucs-vs-glam-county-div-2-county-championship-division-two-2018")
           (target "_self"))
          "GLOUCS vs GLAM - GLAM Won")
         (span
          (@
           (ng-click
            "(direction=='up')?direction = 'down' : direction = 'up'"))
          (a
           (@
            (class "cb-mat-mnu-itm cb-mat-mnu-all")
            (ng-class
             "{true:'cb-mat-mnu-cls'}[direction=='down']")
            (id "match-dropdown"))
           (span
            (@ (ng-bind "(direction=='up') ? 'ALL': 'CLOSE'")))
           (span
            (@
             (ng-class
              "(direction=='up') ? 'cb-caret-down' : 'cb-caret-up'"))))))
        (div
         (@ (id "matchmenu"))
         (div
          (@ (class "cb-mm-wrp {{direction}}"))
          (div
           (@
            (class "cb-scg-drp-dwn cb-col cb-col-100 cb-mnu-{{direction}}"))
           (nav
            (@
             (class "nav cb-mm-nvtb")
             (ng-init "option= 'all'"))
            (a
             (@
              (class "cb-nav-pill-2 {{(option=='all')?'active':''}}")
              (ng-click "option='all'")
              (style "margin-right: 20px;"))
             "All")
            (a
             (@
              (class "cb-nav-pill-2 {{(option=='live')?'active':''}}")
              (ng-click "option='live'")
              (style "margin-right: 20px;"))
             "Live Now")
            (a
             (@
              (class "cb-nav-pill-2 {{(option=='today')?'active':''}}")
              (ng-click "option='today'")
              (style "margin-right: 20px;"))
             "Today"))
           (style ".cb-nt-{{option}}{opacity:0.4!important;}")
           (ul
            (@ (class "cb-scg-drp-dwn-ul"))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div (@ (class "cb-mm-typ")) "T20 LEAGUE")
              (div (@ (class "cb-scg-srs-nm")) "IPL 2018")
              (div
               (@ (class "cb-mtch-all "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
                 (title
                  "Delhi Daredevils v Kings XI Punjab,22nd Match - Live"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Delhi Daredevils vs Kings XI Punjab")
                (span
                 (@ (class "cb-mm-liv-tag"))
                 (& nbsp)
                 (& nbsp)
                 "LIVE"))
               (div (@ (class "cb-mm-mtch-nm")) "22nd Match"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live cb-nt-today"))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/20083/mi-vs-srh-23rd-match-indian-premier-league-2018")
                 (title
                  "Mumbai Indians v Sunrisers Hyderabad,23rd Match - Preview"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Mumbai Indians vs Sunrisers Hyderabad")
                (span (@ (class "cb-mm-liv-tag"))))
               (div (@ (class "cb-mm-mtch-nm")) "23rd Match"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div (@ (class "cb-mm-typ")) "DOMESTIC")
              (div
               (@ (class "cb-scg-srs-nm"))
               "Super Four Provincial Tournament")
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/20155/dambulla-vs-colombo-5th-match-super-four-provincial-tournament-2018")
                 (title
                  "Dambulla v Colombo,5th Match - Stumps"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Dambulla vs Colombo")
                (span (@ (class "cb-mm-liv-tag"))))
               (div (@ (class "cb-mm-mtch-nm")) "5th Match"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/20156/galle-vs-kandy-6th-match-super-four-provincial-tournament-2018")
                 (title "Galle v Kandy,6th Match - Stumps"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Galle vs Kandy")
                (span (@ (class "cb-mm-liv-tag"))))
               (div (@ (class "cb-mm-mtch-nm")) "6th Match"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-scg-srs-nm"))
               "County Division Two")
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19465/gloucs-vs-glam-county-div-2-county-championship-division-two-2018")
                 (title
                  "Gloucestershire v Glamorgan,County Div 2 - GLAM Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Gloucestershire vs Glamorgan")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 2"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19464/derby-vs-mdx-county-div-2-county-championship-division-two-2018")
                 (title
                  "Derbyshire v Middlesex,County Div 2 - DERBY Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Derbyshire vs Middlesex")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 2"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19466/leic-vs-sus-county-div-2-county-championship-division-two-2018")
                 (title
                  "Leicestershire v Sussex,County Div 2 - Match drawn"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Leicestershire vs Sussex")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 2"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live cb-nt-today"))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19463/nor-vs-warks-county-div-2-county-championship-division-two-2018")
                 (title
                  "Northamptonshire v Warwickshire,County Div 2 - WARKS Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Northamptonshire vs Warwickshire")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 2"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-scg-srs-nm"))
               "County Division One")
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19409/sur-vs-ham-county-div-1-county-championship-division-one-2018")
                 (title
                  "Surrey v Hampshire,County Div 1 - SUR Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Surrey vs Hampshire")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 1"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live "))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19410/yorks-vs-notts-county-div-1-county-championship-division-one-2018")
                 (title
                  "Yorkshire v Nottinghamshire,County Div 1 - YORKS Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Yorkshire vs Nottinghamshire")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 1"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live cb-nt-today"))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19407/ess-vs-lancs-county-div-1-county-championship-division-one-2018")
                 (title
                  "Essex v Lancashire,County Div 1 - ESS Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Essex vs Lancashire")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 1"))))
            (li
             (@ (class "cb-lst-mtch cb-lst-dom"))
             " "
             (div
              (@ (style "display:inline-block"))
              (div
               (@ (class "cb-mtch-all cb-nt-live cb-nt-today"))
               (a
                (@
                 (target "_self")
                 (href
                  "/live-cricket-scores/19408/som-vs-worcs-county-div-1-county-championship-division-one-2018")
                 (title
                  "Somerset v Worcestershire,County Div 1 - SOM Won"))
                (span
                 (@ (class "cb-mm-mtch-tm"))
                 "Somerset vs Worcestershire")
                (span (@ (class "cb-mm-liv-tag"))))
               (div
                (@ (class "cb-mm-mtch-nm"))
                "County Div 1"))))))))))
      " "
      (script
       "var matchId = \"20082\";\n"
       "var seriesId = \"2676\";")
      (div
       (@ (id "strip_wrapper") (class "cb-col cb-col-100"))
       (div (@ (id "large_strip") (class "pull-left ad-unit")))
       (div (@ (id "small_strip") (class "pull-right ad-unit")))
       (div
        (@ (id "super_strip") (class "text-center ad-unit"))))
      (div
       (@
        (id "matchCenter")
        (class "cb-col cb-col-100 cb-bg-white")
        (ng-controller "matchCenter"))
       (div (@ (ng-init "matchId = 20082;seriesId = 2676")))
       (div
        (@ (class "cb-nav-main cb-col-100 cb-col cb-bg-white"))
        " "
        (h1
         (@ (class "cb-nav-hdr cb-font-18 line-ht24"))
         "Delhi Daredevils vs Kings XI Punjab, 22nd Match - Live Cricket Score, Commentary")
        " "
        (div
         (@ (class "cb-nav-subhdr cb-font-12"))
         " "
         (span (@ (class "text-bold")) "Series: ")
         " "
         (a
          (@
           (href
            "/cricket-series/2676/indian-premier-league-2018")
           (title "Indian Premier League, 2018"))
          (span
           (@ (class "text-hvr-underline text-gray"))
           "Indian Premier League, 2018"))
         " "
         (span (@ (class "text-bold pad-left")) "Venue: ")
         (a
          (@
           (href
            "/cricket-series/2676/indian-premier-league-2018/venues/51/feroz-shah-kotla")
           (title "Feroz Shah Kotla, Delhi"))
          (span
           (@ (class "text-hvr-underline text-gray"))
           "Feroz Shah Kotla, Delhi"))
         " "
         (span (@ (class "text-bold pad-left")) "Date & Time: ")
         " "
         (span "Apr 23," (& nbsp))
         " "
         (span "08:00 PM")
         " "
         (span (& nbsp) "LOCAL")
         " ")
        " "
        (nav
         (@ (class "cb-nav-bar") (role "navigation"))
         " "
         (a
          (@
           (class "cb-nav-tab active")
           (href
            "/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Live Scores & Commentary"))
          "Commentary")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/live-cricket-scorecard/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Scorecard"))
          "Scorecard")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/cricket-match-highlights/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Highlights")
           (target "_self"))
          "Highlights")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/live-cricket-full-commentary/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Full Commentary"))
          " Full Commentary")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/live-cricket-match-blog/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Live Blog"))
          "Live Blog")
         " "
         (a
          (@
           (class "cb-nav-tab")
           (href
            "/cricket-series/2676/indian-premier-league-2018/points-table")
           (title "Indian Premier League, 2018 Points Table")
           (target "_blank"))
          "Points Table")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/cricket-match-facts/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Match Facts"))
          "Match Facts")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/cricket-match-news/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match News"))
          "News")
         " "
         (a
          (@
           (class "cb-nav-tab ")
           (href
            "/cricket-match-photos/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018")
           (title
            "Delhi Daredevils vs Kings XI Punjab, 22nd Match Photos"))
          "Photos")
         " "))
       (div
        (@ (class "cb-col cb-col-67 cb-nws-lft-col cb-comm-pg"))
        (div
         (@
          (class "cb-mini-scr-col")
          (ng-show "!isMiniscoreRendered"))
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-min-lv cb-mini-col"))
          (div
           (@ (class "cb-col-100 cb-col cb-col-scores"))
           " "
           (div
            (@ (class "cb-col cb-col-67 cb-scrs-wrp"))
            " "
            (div
             (@ (class "cb-text-gray cb-font-16"))
             " KXIP 143/8 (20.0 Ovs) ")
            " "
            (div
             (@ (class "cb-min-bat-rw"))
             " "
             (span
              (@ (class "cb-font-20 text-bold"))
              " DD 126/7 (18.4 Ovs) ")
             " "
             (span
              (@ (class "cb-font-12 cb-text-gray"))
              " "
              (span
               (@ (class "text-bold"))
               (& nbsp)
               (& nbsp)
               "CRR:"
               (& nbsp))
              (span "6.75")
              " ")
             " "
             (span
              (@ (class "cb-font-12 cb-text-gray"))
              " "
              (span
               (@ (class "text-bold"))
               (& nbsp)
               (& nbsp)
               "REQ:"
               (& nbsp))
              (span "13.5")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-text-inprogress"))
             "Delhi Daredevils need 18 runs in 8 balls")
            " ")
           " "
           (div
            (@
             (id "miniscore_branding")
             (class "cb-col cb-col-33 ad-unit miniscore_branding"))))
          (div
           (@ (class "cb-col-67 cb-col"))
           (div
            (@ (class "cb-min-inf cb-col-100"))
            (div
             (@
              (class "cb-col cb-col-100 cb-min-hdr-rw cb-bg-gray"))
             (div (@ (class "cb-col cb-col-50")) "Batsman")
             (div (@ (class "cb-col cb-col-10 text-right")) "R")
             (div (@ (class "cb-col cb-col-10 text-right")) "B")
             (div (@ (class "cb-col cb-col-8 text-right")) "4s")
             (div (@ (class "cb-col cb-col-8 text-right")) "6s")
             (div
              (@ (class "cb-col cb-col-14 text-right"))
              "SR"))
            (div
             (@ (class "cb-col cb-col-100 cb-min-itm-rw"))
             (div
              (@ (class "cb-col cb-col-50"))
              (a
               (@
                (href "/profiles/9428/shreyas-iyer")
                (class "cb-text-link"))
               "Shreyas Iyer"))
             (div
              (@ (class "cb-col cb-col-10 ab text-right"))
              "44")
             (div
              (@ (class "cb-col cb-col-10 ab text-right"))
              "38")
             (div
              (@ (class "cb-col cb-col-8 ab text-right"))
              "4")
             (div
              (@ (class "cb-col cb-col-8 ab text-right"))
              "0")
             (div
              (@ (class "cb-col cb-col-14 ab text-right"))
              "115.79"))
            (div
             (@ (class "cb-col cb-col-100 cb-min-itm-rw"))
             (div
              (@ (class "cb-col cb-col-50"))
              (a
               (@
                (href "/profiles/1454/amit-mishra")
                (class "cb-text-link"))
               "Amit Mishra"))
             (div
              (@ (class "cb-col cb-col-10 ab text-right"))
              "1")
             (div
              (@ (class "cb-col cb-col-10 ab text-right"))
              "1")
             (div
              (@ (class "cb-col cb-col-8 ab text-right"))
              "0")
             (div
              (@ (class "cb-col cb-col-8 ab text-right"))
              "0")
             (div
              (@ (class "cb-col cb-col-14 ab text-right"))
              "100.00")))
           (div
            (@ (class "cb-min-inf cb-col-100"))
            (div
             (@
              (class "cb-col cb-col-100 cb-min-hdr-rw cb-bg-gray"))
             (div (@ (class "cb-col cb-col-50")) "Bowler")
             (div (@ (class "cb-col cb-col-10 text-right")) "O")
             (div (@ (class "cb-col cb-col-8 text-right")) "M")
             (div (@ (class "cb-col cb-col-10 text-right")) "R")
             (div (@ (class "cb-col cb-col-8 text-right")) "W")
             (div
              (@ (class "cb-col cb-col-14 text-right"))
              "ECO"))
            (div
             (@ (class "cb-col cb-col-100 cb-min-itm-rw"))
             (div
              (@ (class "cb-col cb-col-50"))
              (a
               (@
                (href "/profiles/9214/barinder-sran")
                (class "cb-text-link"))
               "Barinder Sran"))
             (div
              (@ (class "cb-col cb-col-10 text-right"))
              "3.4")
             (div (@ (class "cb-col cb-col-8 text-right")) "0")
             (div
              (@ (class "cb-col cb-col-10 text-right"))
              "44")
             (div (@ (class "cb-col cb-col-8 text-right")) "1")
             (div
              (@ (class "cb-col cb-col-14 text-right"))
              "12.00"))
            (div
             (@ (class "cb-col cb-col-100 cb-min-itm-rw"))
             (div
              (@ (class "cb-col cb-col-50"))
              (a
               (@
                (href "/profiles/9617/andrew-tye")
                (class "cb-text-link"))
               "Andrew Tye"))
             (div (@ (class "cb-col cb-col-10 text-right")) "4")
             (div (@ (class "cb-col cb-col-8 text-right")) "0")
             (div
              (@ (class "cb-col cb-col-10 text-right"))
              "25")
             (div (@ (class "cb-col cb-col-8 text-right")) "2")
             (div
              (@ (class "cb-col cb-col-14 text-right"))
              "6.25"))))
          (div
           (@ (class "cb-col cb-col-33 cb-key-st-lst"))
           " "
           (div
            (@ (class "cb-key-lst-wrp cb-font-12 cb-text-gray"))
            " "
            (div
             (@ (class "cb-min-hdr-rw cb-bg-gray"))
             "Key Stats")
            " "
            (div
             (@ (class "cb-min-itm-rw"))
             " "
             (span
              (@ (class "text-bold"))
              "Partnership:"
              (& nbsp))
             " "
             (span "2(2)")
             " ")
            " "
            (div
             (@ (class "cb-min-itm-rw"))
             " "
             (span (@ (class "text-bold")) "Toss:" (& nbsp))
             " "
             (span "Delhi Daredevils(Fielding)")
             " ")
            " "))
          (div
           (@
            (class "cb-col cb-col-100 cb-font-12 cb-text-gray cb-min-rcnt"))
           (span (@ (class "text-bold")) "Recent:" (& nbsp))
           (span
            " 1 1 L1 1 . 1 | 6 1 1 4 2 1 | 1 1 Wd 1 2 1 W | 1 W 1 1 "))))
        (div
         (@
          (ng-show "isMiniscoreRendered")
          (ng-include "'miniscore'")))
        (script
         (@ (id "miniscore") (type "text/ng-template"))
         "<div class=\"cb-col cb-col-100 cb-mini-col cb-bg-white cb-min-lv\" ng-show=\"match.miniscore != null && match.state!='mom'\"> <div class=\"cb-col-100 cb-col cb-col-scores\"><div class=\"cb-col cb-col-67 cb-scrs-wrp\"><h2 class=\"cb-text-gray cb-font-16 text-normal\" ng-bind=\"match.miniscore.bowling.short_name + ' ' + match.miniscore.bowling.score\"></h2><div class=\"cb-min-bat-rw\"><h2 class=\"cb-font-20 text-bold inline-block\" ng-bind=\"match.miniscore.batting.short_name + ' ' + match.miniscore.batting.score\"></h2><span class=\"cb-font-12 cb-text-gray\" ng-show=\"match.miniscore.crr\"><span class=\"text-bold\" ng-bind=\"'&nbsp;&nbsp;CRR:&nbsp;'\"></span><span ng-bind=\"match.miniscore.crr\"></span></span><span class=\"cb-font-12 cb-text-gray\" ng-show=\"match.miniscore.rrr\"><span class=\"text-bold\" ng-bind=\"'&nbsp;&nbsp;REQ:&nbsp;'\"></span><span ng-bind=\"match.miniscore.rrr\"></span></span></div><div class=\"cb-text-{{match.state}}\" ng-bind=\"match.status\"></div></div><div id=\"miniscore_branding\" class=\"cb-col cb-col-33 ad-unit miniscore_branding\"></div></div><div class=\"cb-col-67 cb-col\"><div class=\"cb-min-inf cb-col-100\" ng-if=\"match.miniscore.batsman\"><div class=\"cb-col cb-col-100 cb-min-hdr-rw cb-bg-gray\"><div class=\"cb-col cb-col-50\">Batsman</div><div class=\"cb-col cb-col-10 text-right\">R</div><div class=\"cb-col cb-col-10 text-right\">B</div><div class=\"cb-col cb-col-8 text-right\">4s</div><div class=\"cb-col cb-col-8 text-right\">6s</div><div class=\"cb-col cb-col-14 text-right\">SR</div></div><div class=\"cb-col cb-col-100 cb-min-itm-rw\" ng-repeat=\"batsmen in match.miniscore.batsman\"><div class=\"cb-col cb-col-50\"><a class=\"cb-text-link\" ng-href=\"{{batsmen.get_profile_url()}}\" ng-bind=\"batsmen.full_name\" class=\"\"></a><span ng-if=\"batsmen.strike == 1\" ng-bind=\"' *'\"></span></div><div class=\"cb-col cb-col-10 ab text-right\" ng-bind=\"batsmen.r\"></div><div class=\"cb-col cb-col-10 ab text-right\" ng-bind=\"batsmen.b\"></div><div class=\"cb-col cb-col-8 ab text-right\" ng-bind=\"batsmen['4s']\"></div><div class=\"cb-col cb-col-8 ab text-right\" ng-bind=\"batsmen['6s']\"></div><div class=\"cb-col cb-col-14 text-right\" ng-bind=\"batsmen.sr\"></div></div></div><div class=\"cb-min-inf cb-col-100\" ng-if=\"match.miniscore.bowler\"><div class=\"cb-col cb-col-100 cb-min-hdr-rw cb-bg-gray\"><div class=\"cb-col cb-col-50\">Bowler</div><div class=\"cb-col cb-col-10 text-right\">O</div><div class=\"cb-col cb-col-8 text-right\">M</div><div class=\"cb-col cb-col-10 text-right\">R</div><div class=\"cb-col cb-col-8 text-right\">W</div><div class=\"cb-col cb-col-14 text-right\">ECO</div></div><div class=\"cb-col cb-col-100 cb-min-itm-rw\" ng-repeat=\"bowler in match.miniscore.bowler\" ><div class=\"cb-col cb-col-50\"><a class=\"cb-text-link\" ng-href=\"{{bowler.get_profile_url()}}\" ng-bind=\"bowler.full_name\"></a><span ng-if=\"$first\">&nbsp;*</span></div><div class=\"cb-col cb-col-10 text-right\" ng-bind=\"bowler.o\"></div><div class=\"cb-col cb-col-8 text-right\" ng-bind=\"bowler.m\"></div><div class=\"cb-col cb-col-10 text-right\" ng-bind=\"bowler.r\"></div><div class=\"cb-col cb-col-8 text-right\" ng-bind=\"bowler.w\"></div><div class=\"cb-col cb-col-14 text-right\" ng-bind=\"bowler.er\"></div></div></div></div><div class=\"cb-col cb-col-33 cb-key-st-lst\"><div class=\"cb-key-lst-wrp cb-font-12 cb-text-gray\"><div class=\"cb-min-hdr-rw cb-bg-gray\" ng-bind=\"'Key Stats'\"></div><div class=\"cb-min-itm-rw\" ng-show=\"match.miniscore.partnership\"><span class=\"text-bold\" ng-bind=\"'Partnership:&nbsp;'\"></span><span ng-bind=\"match.miniscore.partnership\"></span></div><div class=\"cb-min-itm-rw\" ng-show=\"match.miniscore.last_wicket\"><span class=\"text-bold\" ng-bind=\"'Last Wkt: '\"></span><span ng-bind=\"match.miniscore.last_wicket.nick_name +' '+ match.miniscore.last_wicket.score\"></span></div><div class=\"cb-min-itm-rw\" ng-show=\"match.miniscore.overs_left\"><span class=\"text-bold\" ng-bind=\"'Ovs Left: '\"></span><span ng-bind=\"match.miniscore.overs_left\"></span></div><div class=\"cb-min-itm-rw\" ng-show=\"match.miniscore.over_summary && match.miniscore.over_summary.over != 0\"><span class=\"text-bold\" ng-bind=\"'Last '+match.miniscore.over_summary.over+' overs: '\"></span><span ng-bind=\"match.miniscore.over_summary.runs + ' runs, ' + match.miniscore.over_summary.wickets + ' wkts'\"></span></div><div class=\"cb-min-itm-rw\" ng-show=\"match.toss.winner\"><span class=\"text-bold\" ng-bind=\"'Toss: '\"></span><span ng-bind=\"match.toss.winner + '&nbsp;(' + match.toss.decision + ')'\"></span></div></div></div><div class=\"cb-col cb-col-100 cb-comm-rcnt-wrap\"><div class=\"cb-col cb-col-100\"><div class=\"cb-col cb-col-100 cb-font-12 cb-text-gray cb-min-rcnt\" ng-if=\"match.miniscore.previous_overs_string\"><span class=\"text-bold\" ng-bind=\"'Recent: '\" ></span><span ng-bind=\"match.miniscore.previous_overs_string\"></span><a ng-if=\"match.hys == 'true'\" onclick=\"attach_recaptcha_js();modal_toggle('hys_modal', 'block');\" class=\"cb-cursor text-bold cb-text-link pull-right\">Have Your Say</a></div></div></div></div><div class=\"cb-col cb-col-100 cb-mini-col cb-min-comp\" ng-show=\"match.state=='mom'\"><div class=\"cb-col cb-col-100 cb-col-scores\"><div class=\"cb-col cb-col-67 cb-scrs-wrp\"><h2 class=\"cb-col cb-col-100 cb-min-tm cb-text-gray\" ng-show=\"match.miniscore.bowling.score\" ng-bind=\"match.miniscore.bowling.short_name + ' ' + match.miniscore.bowling.score\"></h2><h2 class=\"cb-col cb-col-100 cb-min-tm\" ng-show=\"match.miniscore.batting.score\" ng-bind=\"match.miniscore.batting.short_name + ' ' + match.miniscore.batting.score\"></h2></div><div id=\"miniscore_branding\" class=\"cb-col cb-col-33 ad-unit miniscore_branding\"></div></div><div class=\"cb-col cb-col-100 cb-min-stts cb-text-{{match.state}}\" ng-bind=\"match.status\"></div><div class=\"cb-col cb-col-50 cb-mom-itm\" ng-repeat=\"mom in match.mom\"><span class=\"cb-text-gray cb-font-12\">PLAYER OF THE MATCH</span><br><a ng-href=\"{{mom.get_profile_url()}}\" ng-bind=\"mom.full_name\" class=\"cb-link-undrln\"></a></div><div class=\"cb-col cb-col-50 cb-mom-itm\" ng-repeat=\"mos in match.mos\"><span class=\"cb-text-gray cb-font-12\">PLAYER OF THE SERIES</span><br><a ng-href=\"{{mos.get_profile_url()}}\" ng-bind=\"mos.full_name\" class=\"cb-link-undrln\"></a></div></div>")
        (div
         (@
          (class "cb-col cb-col-100 cb-comm-static ")
          (style "min-height:31px;"))
         (div
          (@
           (id "text_link_container")
           (class "cb-col cb-col-100 bg-white ad-native ng-cloak")
           (ad-direct-call "true")
           (ad-loaded "false")
           (ad-has-default "true")
           (ad-repetitive-index "native_commentary_text_link")
           (ad-content
            "<a rel=\"nofollow noreferrer\" href=\"[[clk]]\" target=\"_blank\" class=\"ad-plugin cb-comm-static-anchr\">[[title]]<img src=\"[[logo]]\" height =\"14px\" style=\"margin-left:10px;\"/></a>"))
          (span (@ (class "ad-comm-text-links")))))
        (style
         ".cb-min-vid{background: #000; color:#fff; padding:5px; height:54px; overflow: hidden; margin:0px -10px 0 0;}")
        (div
         (@ (ng-show "!isCommentaryRendered"))
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           "Third man back, fine leg in")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (div
           (@ (class "cb-col cb-col-8 text-bold"))
           " "
           (div
            (@ (title "") (class "cb-mat-mnu-wrp cb-ovr-num"))
            "18.3")
           " ")
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-90"))
           "Barinder to Shreyas Iyer, 1 run, slower ball, swung away in the air and falls short of Finch at long-on. Good thinking from Barinder to bang the slower ones into the pitch and force the batsman to manufacture pace")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           (b
            "Amit Mishra, right handed bat, comes to the crease"))
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           "Batsmen have crossed over, so Shreyas will be back on strike")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (div
           (@ (class "cb-col cb-col-8 text-bold"))
           " "
           (div
            (@ (title "") (class "cb-mat-mnu-wrp cb-ovr-num"))
            "18.2")
           " ")
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-90"))
           "Barinder to Plunkett, "
           (b "out")
           " Caught by Nair!! And what does he do first ball? Advances and connects well, but finds the fielder at deep backward square leg. It was a short slower ball, Plunkett wanted to flick that in the gap wide of deep mid-wicket, but hit it too square and too well. "
           (b "Plunkett c Nair b Barinder 0(1)"))
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           "Barinder to Plunkett, "
           (b "THAT'S OUT!!")
           " Caught!!")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           "Watchout for those slower ball, Plunkett!")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (div
           (@ (class "cb-col cb-col-8 text-bold"))
           " "
           (div
            (@ (title "") (class "cb-mat-mnu-wrp cb-ovr-num"))
            "18.1")
           " ")
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-90"))
           "Barinder to Shreyas Iyer, 1 run, slower ball on the short of length zone, Shreyas pulls and gets an under edge that rolls to mid-wicket")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           (b
            "Liam Plunkett, right handed bat, comes to the crease"))
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100"))
          " "
          (p
           (@ (class "cb-com-ln cb-col cb-col-100"))
           "Andrew Tye finishes 4-0-25-2. 21 needed off 12 balls. Oh ho! It's Barinder again. Was expensive last over. Mujeeb has one over. Shouldn't he be bowling the penultimate over?")
          " ")
         " ")
        " "
        (div
         (@
          (class "html-refresh")
          (url
           "/api/html/expert-comments/expert-comments-multiple-modal/20082")
          (timeout "600000")
          (disable-first-load "true"))
         " "
         (div
          (@
           (id "expert_modal")
           (class "cb-modal-open disp-none")
           (class "ng-cloak"))
          " "
          (div
           (@
            (class "cb-modal-background")
            (onclick "modal_toggle('expert_modal', 'none');")))
          " "
          (div
           (@ (class "cb-expt-cmnt-mdl-wrp"))
           " "
           (div
            (@
             (class "cb-col cb-col-100 cb-stats-dark-bg text-white cb-expt-bar-mdl cb-font-18 text-bold"))
            " Expert Comments - Delhi Daredevils v Kings XI Punjab, 22nd Match "
            (a
             (@
              (class "cb-cursor text-white pull-right")
              (title "close")
              (onclick "modal_toggle('expert_modal', 'none');"))
             "x")
            " ")
           " "
           (div
            (@
             (class "cb-col cb-col-100 text cb-expt-cmnt-lst-wrp cb-expt-cmnt-wrp"))
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div)
             " "
             (div
              (@ (class "cb-text-gray"))
              "Run-out in a tight run chase when you are the last recognized batsman. The thing in favor of Delhi Daredevils is that the asking rate is still only 8.5. This will be about wickets")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Run-out in a tight run chase when you are the last recognized batsman. T...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div)
             " "
             (div
              (@ (class "cb-text-gray"))
              "Tremendous defence of 143 so far from Kings XI Punjab. Remember that old stat. If you lose 3 wickets in the first 6 overs, you lose 3 times out of 4.")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Tremendous defence of 143 so far from Kings XI Punjab. Remember that old...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div)
             " "
             (div
              (@ (class "cb-text-gray"))
              "Real opportunity for Delhi Daredevils to get moving in the IPL today. They might be a batsman short and there is something for the bowlers but they should fancy getting 144.")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Real opportunity for Delhi Daredevils to get moving in the IPL today. Th...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div)
             " "
             (div
              (@ (class "cb-text-gray"))
              "Now, that is an excellent start to his IPL career for Liam Plunkett. Has looked really good. And has put his team in a good spot.")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Now, that is an excellent start to his IPL career for Liam Plunkett. Has...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div "New pace sensation on the bloc")
             " "
             (div
              (@ (class "cb-text-gray"))
              "Like the pace that Avesh Khan generates. And there is something at the Kotla this year for the quicks.")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Like the pace that Avesh Khan generates. And there is something at the K...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div)
             " "
             (div
              (@ (class "cb-text-gray"))
              "Virtually new Delhi Daredevils team out today. A game for Prithvi Shaw, for the pacy Avesh Khan and Amit Mishra. No Jason Roy or Chris Morris but first look at Liam Plunkett")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Virtually new Delhi Daredevils team out today. A game for Prithvi Shaw, ...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-lst cb-expt-cmnt-mdl-lst"))
             " "
             (div
              (@ (class "cb-col cb-col-100 cb-tags"))
              " "
              (div
               (@ (class "cb-col cb-col-10 cb-expt-cmnt-img"))
               " "
               (img
                (@
                 (class "cb-auth-img")
                 (height "50")
                 (width "50")
                 (src
                  "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
               " ")
              " "
              (div
               (@ (class "cb-col cb-col-90 cb-expt-cmnt-nme"))
               " "
               (div
                (@ (class "text-bold cb-font-18"))
                "Harsha Bhogle")
               " "
               (div
                (@ (class "cb-text-italic cb-ln-ht1"))
                "Voice of Cricket")
               " ")
              " ")
             " "
             (div "No Gayle")
             " "
             (div
              (@ (class "cb-text-gray"))
              "Was looking forward to watching Chris Gayle tee-ing off on this pitch with the small boundaries at the Kotla.")
             " "
             (div
              (@ (class "cb-expt-scl-shr"))
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Was looking forward to watching Chris Gayle tee-ing off on this pitch wi...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
                (style "padding-right:15px;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-twtr-blu")))
                " ")
               " "
               (span
                (@
                 (class "cb-font-12 cb-text-link cb-expt-twt"))
                "Tweet")
               " ")
              " "
              (a
               (@
                (class "cb-expt-scl-ancr")
                (target "_blank")
                (href
                 "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
                (rel "noreferrer"))
               " "
               (div
                (@ (class "cb-hys-shr-pad"))
                " "
                (span (@ (class "cb-ico cb-fb-blu")))
                " ")
               " "
               (span
                (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
                "Share")
               " ")
              " ")
             " ")
            " ")
           " ")
          " ")
         " ")
        (div
         (@
          (id "hys_modal")
          (class "cb-modal-open disp-none")
          (class "ng-cloak"))
         " "
         (div
          (@
           (class "cb-modal-background")
           (onclick "modal_toggle('hys_modal', 'none');")))
         (div
          (@ (class "cb-col cb-col-100 cb-expt-cmnt-mdl-wrp"))
          (div
           (@
            (class "cb-col cb-col-100 cb-stats-dark-bg text-white cb-hys-bar"))
           (a
            (@
             (onclick "modal_toggle('hys_modal', 'none');")
             (class "cb-cursor text-white pull-right text-bold cb-hys-cls")
             (title "close"))
            "X")
           (div
            (@ (class "cb-font-18 text-black text-bold"))
            "Have Your Say")
           (div
            (@ (class "cb-font-14"))
            "Best comments will be published by our commentators"))
          (div
           (@
            (class "cb-col-100 cb-col cb-bg-gray")
            (style "padding:10px 15px;")
            (id "expertComments")
            (ng-controller "expertComments"))
           (div
            (@
             (class "disp-none text-green")
             (id "success_form"))
            "Thanks for having your say.")
           (div
            (@ (class "disp-none text-red") (id "error_form"))
            "There are errors in the submitted form.")
           (div
            (@
             (class "disp-none text-red")
             (id "inapt_content"))
            "Inappropriate content used!")
           (form
            (@
             (name "form")
             (id "HUS-form")
             (novalidate)
             (method "post"))
            (div
             (@ (class "cb-col cb-col-100 cb-mat-mnu-wrp"))
             (label (@ (for "Name")) "Name")
             (input
              (@
               (type "name")
               (ng-model "user.name")
               (name "uName")
               (class "cb-col form-control cb-car-inp")
               (id "cb-contact-name")
               (ng-minlength "2")
               (ng-maxlength "50")
               (placeholder "Name")
               (ng-pattern "/^[a-zA-Z ']*$/")
               (required)))
             (div
              (@
               (ng-show
                "form.$submitted || form.uName.$touched"))
              (div
               (@
                (ng-show "form.uName.$error.required")
                (class "text-red"))
               "Name is required.")
              " "
              (div
               (@
                (ng-show "form.uName.$error.maxlength")
                (class "text-red"))
               "Name should be maximum of 50 characters.")
              " "
              (div
               (@
                (ng-show "form.uName.$error.minlength")
                (class "text-red"))
               "Name should be minimum of 2 characters.")
              " "
              (div
               (@
                (ng-show "form.uName.$error.pattern")
                (class "text-red"))
               "This is not a valid Name.")))
            (div
             (@ (class "cb-col cb-col-100 cb-mat-mnu-wrp"))
             (label (@ (for "Message")) "Comment")
             (textarea
              (@
               (class "cb-col form-control cb-car-inp cb-txtarea")
               (ng-model "user.comment")
               (name "uComment")
               (rows "3")
               (maxlength "300")
               (placeholder
                "Your Comment (maximum 300 characters)")
               (required)))
             (div
              (@
               (class "cb-col cb-col-100 cb-text-gray cb-font-12 cb-sub-opning")
               (ng-bind
                "((user.comment.length)?user.comment.length:0 ) + '/300' ")))
             (div
              (@
               (ng-show
                "form.$submitted || form.uComment.$touched"))
              (div
               (@
                (ng-show "form.uComment.$error.required")
                (class "text-red"))
               "Comment is required.")))
            (div
             (@ (class "cb-col cb-col-100 cb-mat-mnu-wrp"))
             " "
             (div
              (@
               (class "g-recaptcha")
               (data-sitekey
                "6LfIRQsTAAAAAI8hPYGZ2WmPkBlrRhPLuPl6W6Zo")))
             (div
              (@
               (id "catpcha_error_msg")
               (class "disp-none text-red"))
              "Please validate the captcha")
             (div
              (@ (class "cb-mr-btn-mar cb-col cb-col-20"))
              (button
               (@
                (type "submit")
                (ng-click "have_your_say(user)")
                (class "cb-more-btn"))
               "Submit")
              " ")
             " "
             (div
              (@
               (ng-show "show_loader")
               (class " cb-col cb-col-20 cb-loader-align"))
              " "
              (div (@ (class "spinner")))
              " "))))))
        (div
         (@ (ng-show "isCommentaryRendered"))
         (time
          (@
           (ng-if
            "match.last_update_time && match.series.category == 'Domestic'")
           (datetime
            "{{match.last_update_time| date:'yyyy-MM-dd'+'T'+'HH:mm:ss': 'UTC' }}Z")
           (class "cb-text-italic text-gray cb-font-12 cb-col cb-col-100 text-right")
           (ng-bind
            "'Last Updated:' + (match.last_update_time|date:' dd MMM, yyyy, HH:mm')")))
         (div
          (@
           (ng-repeat "ts in match.timestamps")
           (ng-init "comm = match.commentary")
           (ng-include "'commentary'")))
         (div
          (@
           (ng-repeat
            "ts in match.full_commentary.timestamps | limitTo : match.full_commentary.counts")
           (ng-init "comm = match.full_commentary.commentary")
           (ng-include "'commentary'"))))
        (script
         (@ (id "commentary") (type "text/ng-template"))
         "<div class=\"cb-com-ovr-sum cb-col\" ng-if=\"comm[ts].evt == 'over-break'\"><div id=\"plugin_container_{{ts}}\" class=\"cb-col cb-col-100 cb-com-ovr-sum-ad ad-native ng-cloak\" ad-direct-call=\"true\" ad-loaded=\"false\" ad-has-default=\"true\" ad-repetitive-index=\"native_commentary_plugin\" ad-content='<a href=\"[[clk]]\" target=\"_blank\" rel =\"nofollow\" timestamp=\"{{ts}}\" class=\"ad-plugin cb-comm-static-anchr cb-comm-static-anchr well well-sm col-xs-12 col-md-12 cb-ovr-brk-well cb-font-14\">[[title]]<img src=\"[[logo]]\" class=\"pull-right\"/></a>' over_num=\"{{comm[ts].o_no}}\" ><a href=\"\" id=\"plugin_{{ts}}\" class=\"ad-plugin cb-comm-static-anchr cb-comm-static-anchr cb-text-link\" timestamp=\"{{ts}}\" target=\"_blank\"></a></div><div ng-if=\"comm[ts].o_no != null\" class=\"cb-col cb-col-100 cb-com-ovr-sum-rw cb-font-12\"><div class=\"cb-col cb-col-8 cb-com-ovr-sum-itm cb-font-18 text-bold\" ng-bind=\"comm[ts].o_no\" style='min-height:33px;'></div><div class=\"cb-col cb-col-25 cb-com-ovr-sum-itm\"><div>Runs Scored: <span class=\"text-bold\" ng-bind=\"comm[ts].o_runs\"></span></div><div class=\"text-bold\" ng-bind=\"comm[ts].o_summary\"></div></div><div class=\"cb-col cb-col-25 cb-com-ovr-sum-itm\"><div ng-bind=\"'Score after ' + comm[ts].o_no + ' overs'\"></div><div class=\"text-bold\" ng-bind=\"comm[ts].bt_tm_name + ' ' + comm[ts].score + '-' + comm[ts].wkts\"></div></div><div class=\"cb-col cb-col-25 cb-com-ovr-sum-itm\"><div ng-repeat=\"batsman in comm[ts].batsman\"><div class=\"cb-col cb-col-67\" ng-bind=\"batsman.name\"></div><div class=\"cb-col cb-col-33 text-right\" ng-bind=\"' ' + batsman.r + '(' + batsman.b +')'\"></div></div></div><div class=\"cb-col cb-col-17 cb-com-ovr-sum-itm\"><div ng-repeat=\"bowler in comm[ts].bowler\"><div ng-bind=\"bowler.name\" title=\"{{bowler.name}}\"></div><div ng-bind=\"' ' + bowler.o + '-' + bowler.m + '-'+ bowler.r + '-'+ bowler.w\"></div></div></div></div></div><div id=\"comm_{{comm[ts].timestamp}}\" class=\"cb-col cb-col-100\" ng-if=\"comm[ts].evt != 'over-break'\"><div ng-if=\"comm[ts].o_no != null || ( comm[ts].videoId != null && comm[ts].evt.indexOf('Plugin') < 0)\" class=\"cb-col cb-col-8 text-bold\"><div ng-bind=\"comm[ts].o_no\" ng-if=\"comm[ts].o_no != null\" title=\"{{comm[ts].timestamp | date:'medium'}}\" class=\"cb-mat-mnu-wrp cb-ovr-num\"></div><label class=\"kaltura-play cb-cursor\" uiconf_id=\"32458402\" entry_id=\"{{comm[ts].videoId}}\" ng-if=\"($root.$GEO.country != 'US' && $root.$GEO.country != 'CA') && comm[ts].videoId != null && comm[ts].evt != 'wicket'\"><div class=\"cb-comm-vid-brdr\" ><div class=\"cb-comm-vid-play\" >&#9658;</div></div></label><label class=\"kaltura-play cb-cursor\" uiconf_id=\"31675141\" entry_id=\"{{comm[ts].videoId}}\" ng-if=\"comm[ts].videoId && comm[ts].evt != 'wicket' && ($root.$GEO.country == 'US' || $root.$GEO.country == 'CA')\"><div class=\"cb-comm-vid-brdr\"><div class=\"cb-comm-vid-play\">&#9658;</div></div></label></div><p ng-bind-html=\"comm[ts].comm\" class=\"cb-com-ln\" ng-class = \"get_comm_class(comm[ts])\" ng-hide=\"comm[ts].geo != null && comm[ts].geo.length > 0 && comm[ts].geo.indexOf($root.$GEO.country) == -1\"></p><p class=\"kaltura-embed cb-font-14 cb-comm-text cb-col cb-col-90\" uiconf_id=\"31675141\" entry_id=\"{{comm[ts].videoId}}\" ng-if=\"comm[ts].videoId && comm[ts].evt == 'wicket' && ($root.$GEO.country == 'US' || $root.$GEO.country == 'CA')\" style=\"height:320px;margin:0 0 0 50px;\"></p><p class=\"kaltura-embed cb-font-14 cb-comm-text cb-col cb-col-90\" uiconf_id=\"32458402\" entry_id=\"{{comm[ts].videoId}}\" ng-if=\"comm[ts].videoId && comm[ts].evt == 'wicket' && ($root.$GEO.country != 'US' && $root.$GEO.country != 'CA')\" style=\"height:320px;margin:0 0 0 50px;\"></p><p class=\"kaltura-embed cb-font-14 cb-comm-text cb-col cb-col-90 plugin-video\" uiconf_id=\"33630912\" entry_id=\"{{comm[ts].videoId}}\" ng-if=\"comm[ts].videoId && comm[ts].evt == 'Plugin:video' && !comm[ts].videoId\" style=\"height:320px;margin:0 0 0 50px;\"></p><p class=\"video-embed cb-font-14 cb-comm-text cb-col cb-col-90 plugin-video\" ng-if=\"comm[ts].cbvideoId && comm[ts].evt == 'Plugin:video'\" video_id=\"{{comm[ts].videoId}}\" video_url=\"{{comm[ts].cbvideoId}}\" style=\"height:320px;margin:0 0 10px 50px;\"></p></div>")
        (a
         (@
          (id "full_commentary_btn")
          (href "Javascript:void(0);")
          (class "cb-col cb-col-100 cb-com-lod-mr ng-cloak")
          (ng-show "match.timestamps.length > 10")
          (ng-click "fetch_fullcommentary();"))
         "Load More Commentary"))
       (div
        (@ (class "cb-col cb-col-33 cb-col-rt"))
        (div
         (@
          (id "mpu")
          (class "ad-unit cb-col cb-col-100")
          (style "min-height:250px;")))
        (div
         (@ (class "cb-nav-hdr cb-col cb-col-100"))
         (div
          (@ (class "cb-col cb-col-100"))
          (h4 (@ (class "cb-mdl-hdr")) "PLAYER SEARCH")
          (div
           (@
            (id "playerSearchWidget")
            (ng-controller "playerSearchWidget")
            (class "cb-plyr-srch-wrap cb-related-module cb-col cb-col-100")
            (ng-init
             "searchText=''; itemHeight=57; boxHeight=250")
            (style "margin:10px 0;")
            (ng-init "init()"))
           (form
            (@
             (name "form")
             (method "POST")
             (action "/profiles")
             (novalidate)
             (ng-submit "submit_search($event)")
             (ng-cloak))
            " "
            (div
             (@ (class "cb-plyr-srch-inp-wrap"))
             " "
             (div
              (@ (class "cb-plyr-srch-inp-col"))
              " "
              (input
               (@
                (class "cb-plyr-inp-new")
                (name "cbserarch")
                (id "player_search_input")
                (type "text")
                (ng-model "searchText")
                (placeholder "Search Player")
                (pattern ".{2,}")
                (ng-minlength "2")
                (autocomplete "off")
                (ng-keydown "onKeyPress($event)")
                (ng-focus "showDropdown()")))
              " "
              (div
               (@
                (ng-show "dropdownVisible")
                (class "cb-plyr-srch-backdrop")
                (ng-click "hideDropdown()")))
              " "
              (div
               (@
                (ng-show
                 "dropdownVisible && searchText.length > 1")
                (id "playerSearchBox")
                (class "cb-plyr-srch-box"))
               " "
               (p
                (@ (ng-if "filtered_data.length == 0"))
                "Loading...")
               " "
               (a
                (@
                 (class "cb-plyr-srch-box-item {{selectedIndex == $index?'active':''}}")
                 (ng-href "{{results.url}}")
                 (ng-repeat
                  "results in filtered_data track by $index")
                 (ng-if "filtered_data.length > 0")
                 (data-index "{{$index}}")
                 (ng-show "searchText.length >=2")
                 (ng-mouseover "onHoverItem($event)"))
                " "
                (div
                 (@ (class "cb-plyr-srch-item-image"))
                 " "
                 (img
                  (@
                   (class "img-responsive cb-srch-plyr-img")
                   (ng-cloak)
                   (ng-attr-title "{{results.name}}")
                   (ng-attr-alt "{{results.name}}")
                   (ng-src
                    "//i.cricketcb.com/stats/img/faceImages/{{results.id}}.jpg")
                   (style "border-radius:50%;")))
                 " ")
                " ")
               (p
                (@
                 (class "cb-plyr-srch-item-text")
                 (ng-bind "results.name")))
               " "
               " ")
              " ")
             " "
             (button
              (@
               (class "cb-srch-btn text-white")
               (type "submit")
               (ng-click "form.$submitted = true"))
              "GO")
             " ")
            (div
             (@
              (ng-show
               "form.$submitted || form.cbserarch.$touched"))
             (div
              (@
               (ng-show
                "form.cbserarch.$error.minlength || searchText.length < 2")
               (class "text-red"))
              "Please enter atleast 2 characters."))))))
        (div
         (@ (id "mpu2") (class "ad-unit cb-col cb-nav-hdr")))
        " "
        (div
         (@
          (class "html-refresh")
          (url
           "/api/html/expert-comments/expert-comments-multiple/20082")
          (timeout "600000")
          (disable-first-load "true"))
         " "
         (div
          (@ (class "cb-col cb-expt-cmnt-wdgt"))
          " "
          (div
           (@
            (class "cb-col cb-col-100 cb-stats-dark-bg text-white cb-expt-bar"))
           " "
           (div
            (@ (class "cb-font-16 text-bold"))
            "Expert Comments")
           " "
           (div
            (@ (class "cb-text-italic"))
            "Delhi Daredevils v Kings XI Punjab, 22nd Match")
           " ")
          " "
          (div
           (@
            (class "cb-col cb-col-100 cb-expt-cmnt-wdgt-lst-wrp cb-expt-cmnt-wrp"))
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")))
            " "
            (div
             (@ (class "cb-text-gray"))
             "Run-out in a tight run chase when you are the last recognized batsman. The thing in favor of Delhi Daredevils is that the asking rate is still only 8.5. This will be about wickets")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Run-out in a tight run chase when you are the last recognized batsman. T...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")))
            " "
            (div
             (@ (class "cb-text-gray"))
             "Tremendous defence of 143 so far from Kings XI Punjab. Remember that old stat. If you lose 3 wickets in the first 6 overs, you lose 3 times out of 4.")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Tremendous defence of 143 so far from Kings XI Punjab. Remember that old...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")))
            " "
            (div
             (@ (class "cb-text-gray"))
             "Real opportunity for Delhi Daredevils to get moving in the IPL today. They might be a batsman short and there is something for the bowlers but they should fancy getting 144.")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Real opportunity for Delhi Daredevils to get moving in the IPL today. Th...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")))
            " "
            (div
             (@ (class "cb-text-gray"))
             "Now, that is an excellent start to his IPL career for Liam Plunkett. Has looked really good. And has put his team in a good spot.")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Now, that is an excellent start to his IPL career for Liam Plunkett. Has...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div
             (@ (class "cb-expt-cmnt-hdr"))
             "New pace sensation on the bloc")
            " "
            (div
             (@ (class "cb-text-gray"))
             "Like the pace that Avesh Khan generates. And there is something at the Kotla this year for the quicks.")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Like the pace that Avesh Khan generates. And there is something at the K...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")))
            " "
            (div
             (@ (class "cb-text-gray"))
             "Virtually new Delhi Daredevils team out today. A game for Prithvi Shaw, for the pacy Avesh Khan and Amit Mishra. No Jason Roy or Chris Morris but first look at Liam Plunkett")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Virtually new Delhi Daredevils team out today. A game for Prithvi Shaw, ...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " "
           (div
            (@ (class "cb-expt-cmnt-wdgt-lst cb-expt-cmnt-lst"))
            " "
            (div
             (@ (class "cb-col cb-col-100 cb-expt-cmnt-hdr"))
             " "
             (div
              (@ (class "cb-col cb-col-16 cb-expt-cmnt-img"))
              " "
              (img
               (@
                (class "cb-auth-img")
                (height "40")
                (width "40")
                (alt "")
                (src
                 "//i.cricketcb.com/i/news/fth/90x90/images/authorImages//170.jpg")))
              " ")
             " "
             (div
              (@ (class "cb-col cb-col-84 cb-expt-cmnt-nme"))
              " "
              (div (@ (class "text-bold")) "Harsha Bhogle")
              " "
              (div
               (@ (class "cb-font-12 cb-text-italic"))
               "Voice of Cricket")
              " ")
             " ")
            " "
            (div (@ (class "cb-expt-cmnt-hdr")) "No Gayle")
            " "
            (div
             (@ (class "cb-text-gray"))
             "Was looking forward to watching Chris Gayle tee-ing off on this pitch with the small boundaries at the Kotla.")
            " "
            (div
             (@ (class "cb-expt-scl-shr"))
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://twitter.com/intent/tweet?url=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;text=Harsha Bhogle: Was looking forward to watching Chris Gayle tee-ing off on this pitch wi...&amp;hashtags=DDvKXIP&amp;via=cricbuzz")
               (style "padding-right:15px;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-twtr-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-twt"))
               "Tweet")
              " ")
             " "
             (a
              (@
               (class "cb-expt-scl-ancr")
               (target "_blank")
               (href
                "https://www.facebook.com/sharer.php?u=http://www.cricbuzz.com/live-cricket-scores/20082/dd-vs-kxip-22nd-match-indian-premier-league-2018&amp;")
               (rel "noreferrer"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-fb-blu ")))
               " ")
              " "
              (span
               (@ (class "cb-font-12 cb-text-link cb-expt-fb"))
               "Share")
              " ")
             " "
             (a
              (@
               (onclick
                "attach_recaptcha_js();modal_toggle('hys_modal', 'block');")
               (class "cb-cursor")
               (style "padding-right:15px;"))
              " "
              (div
               (@ (class "cb-hys-shr-pad"))
               " "
               (span (@ (class "cb-ico cb-hys-logo")))
               " ")
              " "
              (span
               (@
                (class "cb-font-12 cb-text-link")
                (style "margin-left: 3px;"))
               "Reply")
              " ")
             " ")
            " ")
           " ")
          " ")
         " ")
        " "
        (div
         (@
          (class "cb-col cb-col-100 cb-sr-hist-pad")
          (id "latest-vid-mod")
          (gtm-label "Commentary"))
         " "
         (h4 (@ (class "cb-mdl-hdr")) "Trending Videos")
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-mid-wrp"))
          " "
          (a
           (@
            (href
             "/cricket-videos/27744/cricbuzz-live-dd-vs-kxip-mid-innings-show")
            (class "suggested-video-gtm")
            (gtm-label
             "|Cricbuzz LIVE: DD vs KXIP Mid-innings show|27744")
            (title
             "Cricbuzz LIVE: DD vs KXIP Mid-innings show"))
           " "
           (div
            (@ (class "cb-pos-rel"))
            " "
            (img
             (@
              (class "cb-suggested-vid-img suggested-video-gtm")
              (gtm-label
               "|Cricbuzz LIVE: DD vs KXIP Mid-innings show|27744")
              (alt "Cricbuzz LIVE: DD vs KXIP Mid-innings show")
              (gtm-label
               "|Cricbuzz LIVE: DD vs KXIP Mid-innings show|27744")
              (title
               "Cricbuzz LIVE: DD vs KXIP Mid-innings show")
              (src
               "//api.cricbuzz.com/a/img/v1/290x164/i1/c144531/cricbuzz-live-dd-vs-kxip-mid.jpg")
              (height "164")
              (width "290")))
            " "
            (div
             (@ (class "cb-cen"))
             " "
             (div (@ (class "cb-vid-sm-ply-api")) "►")
             " ")
            " ")
           " "
           (h4
            (@
             (class "text-hvr-underline suggested-video-gtm")
             (gtm-label
              "|Cricbuzz LIVE: DD vs KXIP Mid-innings show|27744"))
            "Cricbuzz LIVE: DD vs KXIP Mid-innings show")
           " ")
          " "
          (div (@ (class "cb-nws-time")) "\t1h ago ")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-tms-itm cb-mid-wrp"))
          " "
          (a
           (@
            (href
             "/cricket-videos/27743/cricbuzz-live-dd-vs-kxip-pre-match-show")
            (class "suggested-video-gtm")
            (gtm-label
             "|Cricbuzz LIVE: DD vs KXIP Pre-match show|27743")
            (title "Cricbuzz LIVE: DD vs KXIP Pre-match show"))
           " "
           (div
            (@ (class "cb-pos-rel"))
            " "
            (img
             (@
              (class "cb-suggested-vid-img suggested-video-gtm")
              (gtm-label
               "|Cricbuzz LIVE: DD vs KXIP Pre-match show|27743")
              (alt "Cricbuzz LIVE: DD vs KXIP Pre-match show")
              (gtm-label
               "|Cricbuzz LIVE: DD vs KXIP Pre-match show|27743")
              (title "Cricbuzz LIVE: DD vs KXIP Pre-match show")
              (src
               "//api.cricbuzz.com/a/img/v1/290x164/i1/c144524/cricbuzz-live-dd-vs-kxip-pre.jpg")
              (height "164")
              (width "290")))
            " "
            (div
             (@ (class "cb-cen"))
             " "
             (div (@ (class "cb-vid-sm-ply-api")) "►")
             " ")
            " ")
           " "
           (h4
            (@
             (class "text-hvr-underline suggested-video-gtm")
             (gtm-label
              "|Cricbuzz LIVE: DD vs KXIP Pre-match show|27743"))
            "Cricbuzz LIVE: DD vs KXIP Pre-match show")
           " ")
          " "
          (div (@ (class "cb-nws-time")) "\t3h ago ")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-tms-itm cb-mid-wrp"))
          " "
          (a
           (@
            (href "/cricket-videos/27737/dd-vs-kxip-preview")
            (class "suggested-video-gtm")
            (gtm-label "|DD vs KXIP Preview|27737")
            (title "DD vs KXIP Preview"))
           " "
           (div
            (@ (class "cb-pos-rel"))
            " "
            (img
             (@
              (class "cb-suggested-vid-img suggested-video-gtm")
              (gtm-label "|DD vs KXIP Preview|27737")
              (alt "DD vs KXIP Preview")
              (gtm-label "|DD vs KXIP Preview|27737")
              (title "DD vs KXIP Preview")
              (src
               "//api.cricbuzz.com/a/img/v1/290x164/i1/c144510/dd-vs-kxip-preview.jpg")
              (height "164")
              (width "290")))
            " "
            (div
             (@ (class "cb-cen"))
             " "
             (div (@ (class "cb-vid-sm-ply-api")) "►")
             " ")
            " ")
           " "
           (h4
            (@
             (class "text-hvr-underline suggested-video-gtm")
             (gtm-label "|DD vs KXIP Preview|27737"))
            "DD vs KXIP Preview")
           " ")
          " "
          (div (@ (class "cb-nws-time")) "\t22h ago ")
          " ")
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-more-btn-cntnr"))
          " "
          (a
           (@
            (title "Click to view more videos")
            (href "/cricket-videos")
            (class "cb-more-btn")
            (role "button"))
           "More Videos")
          " ")
         " ")
        " "
        (div (@ (id "mpu3") (class "ad-unit cb-col")))
        " "
        (div
         (@
          (class "cb-col cb-col-100 cb-sr-hist-pad")
          (id "latest-news-mod")
          (gtm-label "commentary"))
         " "
         (h4 (@ (class "cb-ltst-hdr")) "LATEST NEWS")
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144537/bhuvneshwar-hasnt-travelled-w.jpg")))
           (a
            (@
             (href
              "/cricket-news/101722/ipl-2018-mumbai-indians-sunrisers-hyderabad-look-to-fix-issues-revive-fortunes")
             (title
              " MI, SRH look to fix issues, revive fortunes"))
            (img
             (@
              (height "77")
              (width "100")
              (alt
               "Bhuvneshwar hasn&#39;t travelled with the SRH team to Mumbai.")
              (title
               "Bhuvneshwar hasn&#39;t travelled with the SRH team to Mumbai.")
              (itemprop "image")
              (class "cb-lst-img lazy-loading")
              (source
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144537/bhuvneshwar-hasnt-travelled-w.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-news/101722/ipl-2018-mumbai-indians-sunrisers-hyderabad-look-to-fix-issues-revive-fortunes")
              (title
               " MI, SRH look to fix issues, revive fortunes"))
             " MI, SRH look to fix issues, revive fortunes"))
           (div (span (@ (class "cb-nws-time")) "31m ago"))))
         " "
         (span
          (@
           (id "native_latest_news")
           (class "ad-native ng-cloak")
           (ad-loaded "false")
           (ad-content
            "<div class=\"cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm\" style=\"border-bottom: 1px solid #ecebeb\"><div class=\"cb-col cb-col-33\"><a rel=\"noreferrer\" target=\"_blank\" href=\"[[clk]]\"><img style=\"width:90px;height:70px;\" src=\"[[img]]\" ></a></div><div class=\"cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container\"><a rel=\"nofollow\" target=\"_blank\" href=\"[[clk]]\" class=\"cb-nws-hdln-ancr text-hvr-underline\"><div class=\"cb-list-intro-text\" style=\"height: 66px; overflow: hidden;\">[[text]]</div><div class=\"cb-list-sub-text ad-native-sponsor\">[[sponsor]]<img src=\"[[logo]]\" class=\"pull-right\" style=\"height:14px;\" /></div></a><div style=\"float:right;margin-top:-85px;\"><a rel=\"noreferrer\" target=\"_blank\" href=\"[[adchoicesClickURL]]\"><img src=\"[[adchoicesImgURL]]\"/></a></div></div></div>")))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144530/uae-are-unbeaten-after-three-m.jpg")))
           (a
            (@
             (href
              "/cricket-news/101721/uae-ghana-dominate-wt20-qualifiers-outside-the-circle-cricket-associates")
             (title
              "UAE, Ghana dominate WT20 regional qualifiers"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "UAE are unbeaten after three matches.")
              (title "UAE are unbeaten after three matches.")
              (itemprop "image")
              (class "cb-lst-img lazy-loading")
              (source
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144530/uae-are-unbeaten-after-three-m.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-news/101721/uae-ghana-dominate-wt20-qualifiers-outside-the-circle-cricket-associates")
              (title
               "UAE, Ghana dominate WT20 regional qualifiers"))
             "UAE, Ghana dominate WT20 regional qualifiers"))
           (div (span (@ (class "cb-nws-time")) "2h ago"))))
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144526/sleep-played-14-test-matches-f.jpg")))
           (a
            (@
             (href
              "/cricket-news/101720/sri-lanka-cricket-hire-peter-sleep-to-conduct-two-week-spin-bowling-clinic")
             (title
              "Sri Lanka look to Sleep to address spin bowling woes"))
            (img
             (@
              (height "77")
              (width "100")
              (alt
               "Sleep played 14 Test matches for Australia in a stop-start career")
              (title
               "Sleep played 14 Test matches for Australia in a stop-start career")
              (itemprop "image")
              (class "cb-lst-img lazy-loading")
              (source
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144526/sleep-played-14-test-matches-f.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-news/101720/sri-lanka-cricket-hire-peter-sleep-to-conduct-two-week-spin-bowling-clinic")
              (title
               "Sri Lanka look to Sleep to address spin bowling woes"))
             "Sri Lanka look to Sleep to address spin bowling woes"))
           (div (span (@ (class "cb-nws-time")) "2h ago"))))
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144525/ben-coad-starred-in-yorkshire.jpg")))
           (a
            (@
             (href
              "/cricket-news/101719/county-championship-div-1-ben-coad-stars-in-yorkshires-massive-win")
             (title
              "County Championship, Div. 1: Coad stars in Yorkshire&#39;s big win"))
            (img
             (@
              (height "77")
              (width "100")
              (alt
               "Ben Coad starred in Yorkshire&#39;s win with a 10-wicket haul in the match")
              (title
               "Ben Coad starred in Yorkshire&#39;s win with a 10-wicket haul in the match")
              (itemprop "image")
              (class "cb-lst-img lazy-loading")
              (source
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144525/ben-coad-starred-in-yorkshire.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-news/101719/county-championship-div-1-ben-coad-stars-in-yorkshires-massive-win")
              (title
               "County Championship, Div. 1: Coad stars in Yorkshire&#39;s big win"))
             "County Championship, Div. 1: Coad stars in Yorkshire's big win"))
           (div (span (@ (class "cb-nws-time")) "3h ago"))))
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144523/dd-have-lost-their-away-game-t.jpg")))
           (a
            (@
             (href
              "/cricket-news/101718/live-cricket-score-dd-v-s-kxip-match-22-ipl-2018")
             (title
              "Live cricket score - DD v s KXIP, Match 22, IPL 2018"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "DD have lost their away game to KXIP.")
              (title "DD have lost their away game to KXIP.")
              (itemprop "image")
              (class "cb-lst-img lazy-loading")
              (source
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144523/dd-have-lost-their-away-game-t.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-news/101718/live-cricket-score-dd-v-s-kxip-match-22-ipl-2018")
              (title
               "Live cricket score - DD v s KXIP, Match 22, IPL 2018"))
             "Live cricket score - DD v s KXIP, Match 22, IPL 2018"))
           (div (span (@ (class "cb-nws-time")) "4h ago"))))
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-more-btn-cntnr"))
          " "
          (a
           (@
            (title "Click to view more News")
            (href "/cricket-news")
            (class "cb-more-btn")
            (role "button"))
           "More News")
          " ")
         " ")
        " "
        (style
         " .cb-topic-header{margin-left:0px!important;} .cb-list-heading{font-weight:normal!important} ")
        (div (@ (id "mpu4") (class "ad-unit cb-col")))
        " "
        (div
         (@
          (class "cb-col cb-col-100 cb-sr-hist-pad")
          (id "latest-photos-mod")
          (gtm-label "commentary"))
         " "
         (h4 (@ (class "cb-ltst-hdr")) "LATEST PHOTOS")
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144527/dd-vs-kxip-match-22-ipl-2018.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4148/dd-vs-kxip-match-22-ipl-2018")
             (title "DD vs KXIP, Match 22, IPL 2018"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "DD vs KXIP, Match 22, IPL 2018")
              (title "DD vs KXIP, Match 22, IPL 2018")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144527/dd-vs-kxip-match-22-ipl-2018.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4148/dd-vs-kxip-match-22-ipl-2018")
              (title "DD vs KXIP, Match 22, IPL 2018"))
             "DD vs KXIP, Match 22, IPL 2018"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Mon, Apr 23 2018"))))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144494/rr-vs-mi-match-21-ipl-2018.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4147/rr-vs-mi-match-21-ipl-2018")
             (title "RR vs MI, Match 21, IPL 2018"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "RR vs MI, Match 21, IPL 2018")
              (title "RR vs MI, Match 21, IPL 2018")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144494/rr-vs-mi-match-21-ipl-2018.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4147/rr-vs-mi-match-21-ipl-2018")
              (title "RR vs MI, Match 21, IPL 2018"))
             "RR vs MI, Match 21, IPL 2018"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Sun, Apr 22 2018"))))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144468/srh-vs-csk-match-20-ipl-2018.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4146/srh-vs-csk-match-20-ipl-2018")
             (title "SRH vs CSK, Match 20, IPL 2018"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "SRH vs CSK, Match 20, IPL 2018")
              (title "SRH vs CSK, Match 20, IPL 2018")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144468/srh-vs-csk-match-20-ipl-2018.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4146/srh-vs-csk-match-20-ipl-2018")
              (title "SRH vs CSK, Match 20, IPL 2018"))
             "SRH vs CSK, Match 20, IPL 2018"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Sun, Apr 22 2018"))))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144440/rcb-v-dd-match-19-ipl-2018.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4145/rcb-v-dd-match-19-ipl-2018")
             (title "RCB v DD, Match 19, IPL 2018"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "RCB v DD, Match 19, IPL 2018")
              (title "RCB v DD, Match 19, IPL 2018")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144440/rcb-v-dd-match-19-ipl-2018.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4145/rcb-v-dd-match-19-ipl-2018")
              (title "RCB v DD, Match 19, IPL 2018"))
             "RCB v DD, Match 19, IPL 2018"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Sat, Apr 21 2018"))))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144396/csk-vs-rr-ipl-2018-match-17.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4141/csk-vs-rr-ipl-2018-match-17")
             (title "CSK vs RR, IPL 2018, Match 17"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "CSK vs RR, IPL 2018, Match 17")
              (title "CSK vs RR, IPL 2018, Match 17")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144396/csk-vs-rr-ipl-2018-match-17.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4141/csk-vs-rr-ipl-2018-match-17")
              (title "CSK vs RR, IPL 2018, Match 17"))
             "CSK vs RR, IPL 2018, Match 17"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Fri, Apr 20 2018"))))
         " "
         (div
          (@
           (class "cb-col cb-col-100 cb-lst-itm cb-lst-itm-sm"))
          (div
           (@
            (class "cb-col cb-col-33")
            (itemscope)
            (itemtype "http://schema.org/ImageObject")
            (itemprop "image"))
           (meta (@ (itemprop "width") (content "100")))
           " "
           (meta (@ (itemprop "height") (content "77")))
           " "
           (meta
            (@
             (itemprop "url")
             (content
              "http://api.cricbuzz.com/a/img/v1/100x77/i1/c144396/csk-vs-rr-ipl-2018-match-17.jpg")))
           (a
            (@
             (href
              "/cricket-gallery/4139/csk-vs-rr-ipl-2018-match-17")
             (title "CSK vs RR, IPL 2018, Match 17"))
            (img
             (@
              (height "77")
              (width "100")
              (alt "CSK vs RR, IPL 2018, Match 17")
              (title "CSK vs RR, IPL 2018, Match 17")
              (itemprop "image")
              (class "cb-lst-img")
              (src
               "//api.cricbuzz.com/a/img/v1/100x77/i1/c144396/csk-vs-rr-ipl-2018-match-17.jpg")
              (style "padding:0px;")))))
          (div
           (@
            (class "cb-col-67 cb-nws-lst-rt cb-col cb-col-text-container"))
           (div
            (@ (class "cb-ltst-wgt-hdr"))
            (a
             (@
              (class "cb-nws-hdln-ancr text-hvr-underline")
              (href
               "/cricket-gallery/4139/csk-vs-rr-ipl-2018-match-17")
              (title "CSK vs RR, IPL 2018, Match 17"))
             "CSK vs RR, IPL 2018, Match 17"))
           (div
            (span
             (@ (class "cb-nws-time"))
             "Fri, Apr 20 2018"))))
         " "
         (div
          (@ (class "cb-col cb-col-100 cb-more-btn-cntnr"))
          " "
          (a
           (@
            (title "Click to view more Photos")
            (href "/cricket-photo-gallery")
            (class "cb-more-btn"))
           "More Photos")
          " ")
         " ")
        " "
        (div
         (@
          (id "mpu5")
          (class "ad-unit sticky-container")
          (marker "#sticky-anchor")
          (append-class "stick")
          (stick-content "#mpu5")))
        (div (@ (id "sticky-anchor") (class "cb-col")))
        (div
         (@
          (class "modal fade in")
          (id "authPanel")
          (data-keyboard "true")
          (tabindex "-1")
          (role "dialog")
          (aria-labelledby "authPanel")
          (ng-controller "authPanel"))
         (div
          (@
           (class "modal-backdrop fade in")
           (style "height: 1010px;")))
         (div
          (@ (class "modal-dialog") (role "document"))
          (div
           (@ (class "modal-content"))
           (div
            (@ (class "modal-body") (style "padding:0px 15px;"))
            (a
             (@
              (class "cb-modal-close-btn cbmodals")
              (modalid "authPanel")
              (title "close"))
             (label
              (@ (class " cb-cht-lgn-cls btn-md"))
              (span (@ (class "text-bold cb-font-16")) "X")))
            (div
             (@ (class "") (style "padding:10px 0px;"))
             (div
              (@
               (class " text-center center-block text-bold cb-font-18 cb-cht-lgn-col"))
              "Use one of these to sign in to cricbuzz")
             (div
              (@ (class "text-center"))
              (a
               (@
                (id "authPanel_FB_btn")
                (href "Javascript:void(0);")
                (class "cb-cht-fb-btn login-btn text-white")
                (title "Login using facebook")
                (style "padding:6px 12px;"))
               (span (@ (class "cb-ico cb-social-fb")) (& nbsp))
               (strong "Sign in with Facebook")))
             (div
              (@
               (class "text-center")
               (style "margin-top:20px;"))
              (a
               (@
                (id "authPanel_google_btn")
                (href "Javascript:void(0);")
                (class "cb-cht-gp-btn login-btn text-white")
                (title "Login using google account")
                (style "padding:6px 30px 6px 15px;"))
               (span
                (@ (class "cb-ico cb-social-gplus"))
                (& nbsp))
               (strong "Sign in with Google"))))
            (div
             (@ (class ""))
             (div
              (@
               (class " text-center center-block")
               (style "margin-top:5px;padding:10px 0px;")
               (ng-show "show_loader"))
              (div (@ (class "spinner")))))
            (div
             (@ (class "") (ng-show "error_message !=''"))
             (div
              (@
               (ng-bind "error_message")
               (class " text-center text-red1"))))))))
        (div
         (@
          (class "modal fade in")
          (id "chatNickname")
          (tabindex "-1")
          (role "dialog")
          (aria-labelledby "chatNickname")
          (ng-controller "chatNickname"))
         (div
          (@
           (class "modal-backdrop fade in")
           (style "height: 1010px;")))
         (div
          (@ (class "modal-dialog") (role "document"))
          (div
           (@ (class "modal-content"))
           (div
            (@ (class "modal-body") (style "padding:0px 15px;"))
            (a
             (@
              (class "cb-modal-close-btn cbmodals")
              (modalid "chatNickname")
              (title "close"))
             (label
              (@ (class "cb-cht-lgn-cls btn-md"))
              (span (@ (class "text-bold cb-font-16")) "X")))
            (div
             (@
              (class "cb-col-100 cb-chat-div")
              (style "padding:20px 0;"))
             (div
              (@ (class "cb-cht-lgn-usrs"))
              (span
               (@ (class "text-bold cb-font-18"))
               "New Users")
              (br)
              (span
               (@ (class "cb-font-14"))
               "Choose a nickname"))
             (div
              (@ (class "") (style "margin-bottom:0px"))
              (form
               (@ (ng-submit "check_nickname()"))
               (div
                (@ (class "form-group"))
                (input
                 (@
                  (type "text")
                  (class "form-control cb-cht-inpt")
                  (id "nickname")
                  (ng-model "nickname")
                  (placeholder "Nickname"))))
               (div
                (@ (class "form-group"))
                (a
                 (@
                  (href "javascript:void(0);")
                  (class " cb-more-btn btn-chat-avail")
                  (ng-click "check_nickname()")
                  (title "check nickname available?"))
                 "Check Availability"))
               (div
                (@
                 (ng-bind "response.message")
                 (class "")
                 (ng-show "response.message !=''")
                 (ng-class
                  "{'success' : 'text-success', 'error' : 'text-red1'}[response.state]")
                 (style "padding:0px 0px 10px 0px")))
               (div
                (@ (class "form-group") (ng-hide "hide_save"))
                (a
                 (@
                  (href "javascript:void(0);")
                  (class "cb-more-btn")
                  (ng-click "save_nickname();")
                  (title "save account with this nickname"))
                 "Proceed")
                (span
                 (@ (class "text-muted cb-font-12"))
                 (i
                  "(by clicking Proceed you agree to "
                  (a
                   (@
                    (href "/info/chat-policy")
                    (target "_blank"))
                   "cric chat policy")
                  ")"))))))))))
        (div
         (@
          (id "chat-container")
          (ng-controller "ChatController")
          (class "chat-cntnr"))
         (div
          (@
           (id "chat-header")
           (class "chat-hdr ")
           (ng-click "toggle_module()"))
          (strong (@ (class "text-white")) "CRIC CHAT"))
         (div
          (@
           (id "chat-menu-btn")
           (class "chat-hdr-btn")
           (ng-cloak))
          (a
           (@
            (class "cb-chat-btn-big")
            (ng-click "LOGIN('chat')")
            (ng-show "user.role < 2")
            (title "Login to cric chat"))
           "Login")
          (a
           (@
            (class "cb-chat-btn-big")
            (ng-click "LOGOUT('chat')")
            (ng-show "user.role > 1")
            (title "logout from cric chat"))
           "Logout")
          (a
           (@
            (class "cb-chat-btn-sml")
            (ng-click "refresh()")
            (ng-show "toggle_height")
            (title "refresh chat")
            (style "padding:4px 3px 4px 6px;"))
           (span
            (@ (class "cb-font-16 cb-ico cb-chat-refresh"))))
          (a
           (@
            (class "cb-chat-btn-sml")
            (ng-click "toggle_module()")
            (title "toggle chat widget"))
           (span
            (@
             (style "padding:8px;")
             (class "text-bold cb-font-16")
             (ng-class "[toggle_height]")
             (ng-bind "[toggle_height && '-' || '+']")))))
         (div
          (@
           (id "chat-body")
           (class "closed")
           (ng-class
            "{true:'open',false:'closed'}[toggle_height]"))
          (div
           (@ (id "chat-settings"))
           (a
            (@
             (href "/info/contact")
             (id "chat-contact")
             (target "_blank")
             (title "Write to us")
             (class " chat-stngs-ancr"))
            "Write to us")
           (span
            (@ (ng-show "user.role > 1"))
            (& nbsp)
            "|"
            (& nbsp)
            (a
             (@
              (href "javascript:void(0);")
              (ng-click "chat_search='@'+user.nickname")
              (title
               "see only messsages of people who replied to me")
              (class "chat-stngs-ancr "))
             "Replies"))
           (span
            (@ (ng-show "user.role > 2"))
            (& nbsp)
            "|"
            (& nbsp)
            (a
             (@
              (href "javascript:void(0);")
              (ng-click "moderate_user(null,'p')")
              (title "purge all chat messages")
              (class " chat-stngs-ancr"))
             "Purge Chat")))
          (div
           (@
            (id "chat-search")
            (class "chat-srch-main cb-col-100 cb-col"))
           (input
            (@
             (type "text")
             (class "form-control cb-col chat-srch")
             (placeholder "Search")
             (id "chat_search")
             (ng-model "chat_search")
             (style "width:90%")))
           (a
            (@
             (class "cb-col cb-chat-cross")
             (href "javascript:void(0);")
             (ng-show "chat_search.length == 0"))
            (span (@ (class "cb-ico cb-srch-ico"))))
           (a
            (@
             (class "cb-col cb-sub-opning cb-chat-cross")
             (href "javascript:void(0);")
             (ng-show "chat_search.length > 0")
             (ng-click "chat_search = '';"))
            (span (@ (class "cb-srch-ico cb-font-18")) "✖")))
          (div
           (@
            (id "chat-notification")
            (ng-class
             "{true:'open text-red',false:'closed'}[notification != '']")
            (class "cb-col-100 cb-col cb-bg-white cb-plyr-srch-brdr cb-scrd-lft-col cb-pnts-td"))
           (span
            (@ (class "text-red") (ng-bind "notification")))
           (button
            (@
             (class "cb-bg-white pull-right")
             (ng-click "notification = ''")
             (title "close notification"))
            (span (@ (class "")) "✖")))
          (div
           (@
            (id "chat-message-container")
            (ng-class
             "{true:'loggedin',false:'loggedout'}[user.role > 1]"))
           (div
            (@
             (class "chat-message")
             (ng-repeat
              "message in messages | filter:chat_search ")
             (ng-mouseover
              "show_reply_filter(message.i,'show');")
             (ng-mouseleave
              "show_reply_filter(message.i,'hide');"))
            (strong
             (@
              (ng-bind "message.u.n + ' : '")
              (ng-class "{true:'mod-name'}[message.u.r > 2]")
              (ng-click "user_mod_options(message)")))
            (span
             (@
              (ng-bind "message.m")
              (ng-class "message.c")
              (title "{{(message.i) | date:'medium'}}")))
            (div
             (@
              (id "moderation-options-{{message.i}}")
              (class "moderation-options btn-group"))
             (button
              (@
               (class "btn-info cb-cursor")
               (ng-click "moderate_user(message,'d')")
               (title "Delete this message?"))
              "D")
             (button
              (@
               (class "btn-info cb-cursor")
               (ng-click "moderate_user(message,'da')")
               (title
                "Delete all the messages by {{message.u.n}}"))
              "DA")
             (button
              (@
               (class "btn-info cb-cursor")
               (ng-click "moderate_user(message,'bt')")
               (title "Ban {{message.u.n}} temporarily"))
              "BT")
             (button
              (@
               (class "btn-info cb-cursor")
               (ng-click "moderate_user(message,'b')")
               (title "Ban {{message.u.n}} permanently"))
              "B")
             (label
              (@
               (class "btn-info cb-cursor")
               (style "padding:2px;")
               (title "Message to hidden when banned"))
              (input
               (@
                (type "checkbox")
                (ng-model "mod_params.hm")
                (style "vertical-align:bottom;")))
              "hide"))
            (div
             (@
              (id "user-options-{{message.i}}")
              (class "user-options-open")
              (ng-show "user.role > 1"))
             (a
              (@
               (id "user-reply-{{message.i}}")
               (class "disp-none cb-text-complete")
               (href "javascript:void(0);")
               (ng-click "reply(message)")
               (ng-show "message.u.n != user.nickname")
               (title "reply to {{message.u.n}}"))
              (& nbsp)
              " reply "
              (& nbsp)
              "|"
              (& nbsp))
             (a
              (@
               (id "user-filter-{{message.i}}")
               (class "disp-none cb-text-complete")
               (href "javascript:void(0);")
               (ng-click "$parent.chat_search = message.u.n;")
               (title "filter {{message.u.n}} messages"))
              "filter"))))
          (div
           (@
            (id "chat-input-container")
            (ng-class
             "{true:'open',false:'closed'}[user.role > 1]"))
           (form
            (@ (ng-submit "post_message()"))
            (input
             (@
              (type "text")
              (class "form-control cb-col-100 cb-col")
              (placeholder "Message")
              (id "chat_message")
              (ng-model "chat_message")
              (autocomplete "off")))))))
        (script
         "(function(d, s, id) {\tvar js, fjs = d.getElementsByTagName(s)[0];\tif (d.getElementById(id)) return;\tjs = d.createElement(s); js.id = id;\tjs.async = true;\tjs.defer = true;\tjs.src = \"https://connect.facebook.net/en_GB/sdk.js#xfbml=1&version=v2.0&appId=30119633160\";\tsetTimeout(function(){\tfjs.parentNode.insertBefore(js, fjs);\t},300);\n"
         "}(document, 'script', 'facebook-jssdk'));")
        (script
         (@ (type "text/javascript"))
         "(function() {\tvar gcp = document.createElement('script'); gcp.type = 'text/javascript'; gcp.async = true;\tgcp.src = 'https://apis.google.com/js/client:platform.js';\tsetTimeout(function(){\tvar stag = document.getElementsByTagName('script')[0]; stag.parentNode.insertBefore(gcp, stag);\t},400);\t})();")
        (style
         "iframe[id^=\"oauth2relay\"] { position: fixed !important; }")))
      (style
       ".kaltura-play{display:none;}\n"
       ".stick{position:fixed;top:10px;clear:both;}")
      " "
      (span
       (@
        (id "skin_right")
        (class "ad-unit")
        (style
         "overflow:hidden;position:fixed;top:0;left:calc(50% + 490px);margin-left:3px;z-index:-99;")))))
    (footer
     (@
      (id "FooterWraper")
      (itemscope)
      (itemtype "http://schema.org/WPFooter"))
     (div
      (@ (class "cb-footer cb-col-100 cb-col"))
      (div
       (@ (class "cb-ftr-cntnr"))
       (div
        (@ (class "cb-col-25 cb-col"))
        (a
         (@ (class "center-block") (href "/"))
         (div (@ (class "cb-ftr-logo cb-ico")))))
       (div
        (@ (class "cb-col-25 cb-col"))
        (div
         (@ (class "text-left cb-font-16 text-bold"))
         "MOBILE SITE & APPS")
        (ul
         (@ (class "cb-ftr-ul"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (href "http://m.cricbuzz.com")
            (class "text-white"))
           (span (@ (class "cb-mobile-site cb-ico")))
           (span
            (@ (class "cb-footer-list-rt"))
            "m.cricbuzz.com")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "https://play.google.com/store/apps/details?id=com.cricbuzz.android")
            (target "_blank"))
           (span (@ (class "cb-app-android cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "Android")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href "https://itunes.apple.com/app/id360466413")
            (target "_blank"))
           (span (@ (class "cb-app-ios cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "iOS")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "https://www.windowsphone.com/en-us/store/app/cricbuzz/d349db5c-d4e9-4498-ba6f-b9059e452965")
            (target "_blank"))
           (span (@ (class "cb-app-windows cb-ico")))
           (span
            (@ (class "cb-footer-list-rt"))
            "Windows Mobile")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "https://appworld.blackberry.com/webstore/content/64558")
            (target "_blank"))
           (span (@ (class "cb-app-blackberry cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "Blackberry")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "https://chrome.google.com/webstore/detail/cricbuzz/opljecakjchbhhikbeifamamnpcdbgem")
            (target "_blank"))
           (span (@ (class "cb-app-chrome cb-ico")))
           (span
            (@ (class "cb-footer-list-rt"))
            "Chrome Extension")))))
       (div
        (@ (class "cb-col-25 cb-col"))
        (div
         (@ (class "text-left cb-font-16 text-bold"))
         "FOLLOW US ON")
        (ul
         (@ (class "cb-ftr-ul"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href "https://www.facebook.com/cricbuzz")
            (target "_blank"))
           (span (@ (class "cb-social-fb cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "facebook")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href "https://twitter.com/cricbuzz")
            (target "_blank"))
           (span (@ (class "cb-social-twitter cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "twitter")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "https://www.youtube.com/channel/UCSRQXk5yErn4e14vN76upOw")
            (target "_blank"))
           (span (@ (class "cb-social-ytbe cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "youtube")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href "https://plus.google.com/+cricbuzz/")
            (target "_blank"))
           (span (@ (class "cb-social-gplus cb-ico")))
           (span (@ (class "cb-footer-list-rt")) "google+")))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href "https://in.pinterest.com/cricbuzz/")
            (target "_blank"))
           (span (@ (class "cb-social-pinterest cb-ico")))
           (span
            (@ (class "cb-footer-list-rt"))
            "Pinterest")))))
       (div
        (@ (class "cb-col-25 cb-col"))
        (div
         (@ (class "text-left cb-font-16 text-bold"))
         "COMPANY")
        (ul
         (@ (class "cb-ftr-ul"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@ (class "text-white") (href "/careers"))
           "Careers"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@ (class "text-white") (href "/info/advertise"))
           "Advertise"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (rel "nofollow")
            (href "/info/privacy"))
           "Privacy Policy"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (rel "nofollow")
            (href "/info/termsofuse"))
           "Terms of Use"))
         (li
          (@ (class "cb-ftr-lst"))
          (a
           (@
            (class "text-white")
            (href
             "/product-blog/cricbuzz-mobile-apps-tv-ad-cricket-ka-keeda"))
           "Cricbuzz TV Ads"))))
       (div
        (@ (class "cb-col-100 cb-col cb-ftr-cpyrght"))
        (& copy)
        " 2018 Cricbuzz.com, Times Internet Limited. All rights reserved | "
        (a
         (@
          (class "cb-ftr-cpyrght text-hvr-underline")
          (href "http://timesofindia.indiatimes.com/")
          (target "_blank"))
         "The Times of India")
        " | "
        (a
         (@
          (href "http://navbharattimes.indiatimes.com/")
          (target "_blank")
          (class "cb-ftr-cpyrght text-hvr-underline"))
         "Navbharat Times")))))
    (script
     (@ (type "text/javascript"))
     "var script_tag = document.getElementsByTagName('script')[0];\t(function() {\tvar cmin = document.createElement('script'); cmin.type = 'text/javascript'; cmin.async = true;\tcmin.src = '//i.cricketcb.com/statics/site/js/cricbuzz.min.201804091043.js';\tscript_tag.parentNode.insertBefore(cmin, script_tag);\t})();")
    (noscript
     (iframe
      (@
       (src "//www.googletagmanager.com/ns.html?id=GTM-PGNCT7")
       (height "0")
       (width "0")
       (style "display:none;visibility:hidden"))))
    (script
     "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':\tnew Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],\tj=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=\t'//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);\t})(window,document,'script','dataLayer','GTM-PGNCT7');")
    (script
     "window.onerror = function(message, file, line) {\tvar sFormattedMessage = '[' + file + ' (' + line + ')] ' + message;\tdataLayer.push({'EventAction': \"Application\",'EventLabel': sFormattedMessage,'event': 'Exceptions'});\t}")
    " "))))
(define (get l m)
  (cond ((null? (cdr m)) (list-ref l (car m)))
        (else (get (list-ref l (car m)) (cdr m)))))
;(getpos "Mustafizur to Stokes, 2 runs, slanting cutter. Stokes chases, swinging through the line, but with some extra bounce in it, gets a fat outside half sliced over cover doing his best to run back and reach" k '())
(if (equal? " " (get k '(4 3 5 3 9 4 7 3 3 2)))
    (let* ([over (get k '(4 3 5 3 9 4 7 3 3 3 2))])
      (cond 
            [(equal? (last (string->list (get k '(4 3 5 3 9 4 7 3 3 3 2)))) #\1)
             (begin (display (string-append (get k '(4 3 5 3 9 4 7 3 3 3 2)) " - " (get k '(4 3 5 3 9 4 7 3 5 2)) (get k '(4 3 5 3 9 4 7 3 6)) (get k '(4 3 5 3 9 4 7 6))))
                    (newline)
                    (display (string-append (get k '(4 3 5 3 9 4 7 5 3 2 1)))))]
            [(or (not (equal? (last (string->list over)) #\1))
                 (not (equal? (last (string->list over)) #\2))
                 (not (equal? (last (string->list over)) #\3))
                 (not (equal? (last (string->list over)) #\4))
                 (not (equal? (last (string->list over)) #\5))
                 (not (equal? (last (string->list over)) #\6)))
             (begin (display over) (get k '(4 3 5 3 9 4 7 3 3 3 2))
                    (string-append (get k '(4 3 5 3 9 4 7 5 3 3 2)) " - " (get k '(4 3 5 3 9 4 7 5 5 2)) (get k '(4 3 5 3 9 4 7 5 6))))]
            [else (begin (string-append (get k '(4 3 5 3 9 4 7 3 3 3 2)) " - " (get k '(4 3 5 3 9 4 7 3 5 2)) (get k '(4 3 5 3 9 4 7 3 6)) (get k '(4 3 5 3 9 4 7 6)))
                         (string-append (get k '(4 3 5 3 9 4 7 5 3 3 2)) " - " (get k '(4 3 5 3 9 4 7 5 5 2)) (get k '(4 3 5 3 9 4 7 5 6))))]
            ))
    (begin (display (get k '(4 3 5 3 9 4 7 3 3 2)))
           (newline)
           (display (string-append (get k '(4 3 5 3 9 4 7 5 3 3 2)) " - " (get k '(4 3 5 3 9 4 7 5 5 2)) (get k '(4 3 5 3 9 4 7 5 6))))))
    
           