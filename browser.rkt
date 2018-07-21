#lang racket
(require browser)
(require net/url)
(open-url (string->url "http://www.cricbuzz.com"))