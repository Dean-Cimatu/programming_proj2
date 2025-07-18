#lang web-server/insta

(require web-server/servlet-env)

(define (string-blank? s)
  (or (string=? s "") (regexp-match? #px"^\\s*$" s)))

(define (string-contains-ci? haystack needle)
  (string-contains? (string-downcase haystack) (string-downcase needle)))

(define (extract-binding/default sym bindings default)
  (let ([b (assoc sym bindings)])
    (if b (cdr b) default)))
    
(define (list->indexed-list lst)
  (let loop ([i 0] [l lst] [acc '()])
    (if (null? l)
        (reverse acc)
        (loop (+ i 1) (cdr l) (cons (cons i (car l)) acc)))))

(define users
  (list
   (list "fan@example.com" "fanpass" "Alice Fan" "user")
   (list "band@example.com" "bandpass" "The Rockers" "band")))

(define concerts
  (list
   (list "band@example.com" "Rockin' the Night Away" "2025-08-01" "50" #t "The Grand Hall" "Rock")
   (list "band@example.com" "An Evening of Jazz" "2025-08-15" "40" #t "The Blue Note" "Jazz")
   (list "band@example.com" "Summer Pop Festival" "2025-09-10" "60" #f "City Park" "Pop")
   (list "band@example.com" "Acoustic Sunset Session" "2025-09-20" "35" #t "The Rooftop Bar" "Acoustic")
   (list "band@example.com" "Indie Rock Invasion" "2025-10-05" "45" #t "The Underground" "Indie")
   (list "band@example.com" "Metal Mayhem" "2025-10-18" "65" #t "The Forge" "Metal")
   (list "band@example.com" "Funkytown Groove Fest" "2025-11-01" "50" #f "The Disco Palace" "Funk")
   (list "band@example.com" "Classical Dreams" "2025-11-15" "75" #t "Symphony Hall" "Classical")
   (list "band@example.com" "Electronic Odyssey" "2025-11-29" "55" #t "Club Neon" "Electronic")
   (list "band@example.com" "Holiday Rock Ball" "2025-12-12" "80" #t "The Grand Hall" "Rock")
   (list "band@example.com" "New Year's Eve Bash" "2025-12-31" "120" #t "City Arena" "Pop")
   (list "band@example.com" "Winter Blues Night" "2026-01-15" "40" #f "Smokey's Bar" "Blues")
   (list "band@example.com" "Reggae Rhythms" "2026-01-30" "50" #t "Beachside Stage" "Reggae")
   (list "band@example.com" "Valentine's Day Serenade" "2026-02-14" "90" #t "The Love Lounge" "Acoustic")
   (list "band@example.com" "Punk Rock Riot" "2026-03-01" "30" #t "The Anarchy Club" "Punk")
   (list "band@example.com" "Folk Festival" "2026-03-20" "45" #t "Green Valley Park" "Folk")
   (list "band@example.com" "Spring Awakening Concert" "2026-04-10" "60" #t "Botanical Gardens" "Indie")
   (list "band@example.com" "Country Roads Tour" "2026-04-25" "55" #f "The Old Barn" "Country")
   (list "band@example.com" "Symphonic Metal Gala" "2026-05-09" "85" #t "Opera House" "Metal")
   (list "band@example.com" "Summer Kickoff Party" "2026-06-01" "70" #t "City Park" "Pop")))

(define tickets '())
(define next-ticket-id 1001)

(define ticket-types
  (list
   (list "Standard" "50")
   (list "VIP" "100")))


(define (add-user! email password name type)
  (set! users (cons (list email password name type) users)))

(define (add-concert! band-email title date price venue genre)
  (set! concerts (cons (list band-email title date price #t venue genre) concerts)))

(define (find-user email)
  (findf (lambda (u) (string=? (first u) email)) users))

(define (band-concerts band-email)
  (filter (lambda (c) (string=? (first c) band-email)) concerts))

(define (get-concert idx)
  (if (and (integer? idx) (>= idx 0) (< idx (length concerts)))
      (list-ref concerts idx)
      #f))

(define (update-concert! idx new-title new-date new-price new-available new-venue new-genre)
  (when (and (integer? idx) (>= idx 0) (< idx (length concerts)))
    (set! concerts
          (for/list ([c (in-list concerts)]
                     [i (in-naturals)])
            (if (= i idx)
                (list (first c) new-title new-date new-price new-available new-venue new-genre)
                c)))))

(define (delete-concert! idx)
  (when (and (integer? idx) (>= idx 0) (< idx (length concerts)))
    (set! concerts
          (for/list ([c (in-list concerts)]
                     [i (in-naturals)]
                     #:unless (= i idx))
            c))))

(define (add-ticket! concert-idx ticket-type name dob phone address email)
  (define ticket-id next-ticket-id)
  (set! next-ticket-id (+ next-ticket-id 1))
  (set! tickets (cons (list ticket-id concert-idx ticket-type name dob phone address email) tickets))
  ticket-id)

(define (start request)
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (head (title "Tick3tM4s3r"))
        (body
         (h1 "Welcome to Tick3tM4s3r")
         (p "Please log in or create an account.")
         (form ([action ,(embed/url login-handler)] [method "post"])
           (h2 "Login")
           "Email: " (input ([type "text"] [name "email"] [placeholder "fan@example.com"])) (br)
           "Password: " (input ([type "password"] [name "password"] [placeholder "fanpass"])) (br)
           (input ([type "submit"] [value "Login"]))
         )
         (form ([action ,(embed/url register-handler)] [method "post"])
           (h2 "Register")
           "Name: " (input ([type "text"] [name "name"])) (br)
           "Email: " (input ([type "text"] [name "email"])) (br)
           "Password: " (input ([type "password"] [name "password"])) (br)
           "Account Type: "
           (select ([name "type"])
             (option ([value "user"]) "User")
             (option ([value "band"]) "Band")) (br)
           (input ([type "submit"] [value "Register"]))
         )))))))

(define (register-handler request)
  (define name (extract-binding/default 'name (request-bindings request) ""))
  (define email (extract-binding/default 'email (request-bindings request) ""))
  (define password (extract-binding/default 'password (request-bindings request) ""))
  (define type (extract-binding/default 'type (request-bindings request) "user"))
  (cond
    [(or (string-blank? name) (string-blank? email) (string-blank? password)) (start request)]
    [(find-user email) (start request)]
    [else
     (add-user! email password name type)
     (start request)]))

(define (login-handler request)
  (define email (extract-binding/default 'email (request-bindings request) ""))
  (define password (extract-binding/default 'password (request-bindings request) ""))
  (define user (find-user email))
  (if (and user (string=? (second user) password))
      (if (string=? (fourth user) "band")
          (band-dashboard user)
          (user-dashboard user #f))
      (start request)))

(define (search-handler request user)
  (let* ([bindings (request-bindings request)]
         [search-band (extract-binding/default 'band bindings "")]
         [search-venue (extract-binding/default 'venue bindings "")]
         [search-genre (extract-binding/default 'genre bindings "")]
         [search-available (let ([avail-binding (assoc 'available bindings)])
                             (and avail-binding (string=? (cdr avail-binding) "true")))]
         [all-concerts (list->indexed-list concerts)])
    (define results
      (filter
       (lambda (ic)
         (let* ([c (cdr ic)]
                [band-email (first c)]
                [band-user (find-user band-email)]
                [band-name (if band-user (third band-user) "")]
                [available? (fifth c)]
                [venue (sixth c)]
                [genre (seventh c)])
           (and
            (or (string-blank? search-band) (string-contains-ci? band-name search-band))
            (or (string-blank? search-venue) (string-contains-ci? venue search-venue))
            (or (string-blank? search-genre) (string-contains-ci? genre search-genre))
            (if search-available
                available?
                #t))))
       all-concerts))
    (user-dashboard user results)))

(define (user-dashboard user search-results)
  (define user-name (third user))
  (define indexed-concerts (if search-results search-results (list->indexed-list concerts)))
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (head (title "Tick3tM4s3r - Dashboard"))
        (body
         (h1 "Welcome, " ,user-name "!")
         (h2 "Search for Concerts")
         (form ([action ,(embed/url (lambda (req) (search-handler req user)))] [method "post"])
           "Band: " (input ([type "text"] [name "band"])) " "
           "Venue: " (input ([type "text"] [name "venue"])) " "
           "Genre: " (input ([type "text"] [name "genre"])) " "
           (input ([type "checkbox"] [name "available"] [value "true"])) " Only Show Available" (br)
           (input ([type "submit"] [value "Search"]))
           (a ([href ,(embed/url (lambda (req) (user-dashboard user #f)))]) "Clear Search"))
         (h2 "Available Concerts")
         (ul
           ,@(map (lambda (indexed-concert)
                    (define idx (car indexed-concert))
                    (define concert (cdr indexed-concert))
                    (define title (second concert))
                    (define date (third concert))
                    (define price (fourth concert))
                    (define available? (fifth concert))
                    (define venue (sixth concert))
                    (define genre (seventh concert))
                    (define display-text (string-append title " at " venue " - " date " (" genre ") - $" price))
                    `(li ,(if available?
                              `(a ([href ,(embed/url (lambda (req) (select-ticket-page req idx user)))]) ,display-text)
                              `(s ,display-text " (Unavailable)"))))
                  indexed-concerts)))
         (a ([href ,(embed/url (lambda (req) (start req)))]) "Logout"))))))

(define (band-dashboard user)
  (define band-email (first user))
  (define band-name (third user))
  (define indexed-concerts (list->indexed-list concerts))
  (define my-indexed-listings
    (filter (lambda (ic)
              (string=? band-email (first (cdr ic))))
            indexed-concerts))
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (head (title "Tick3tM4s3r - Band Dashboard"))
        (body
         (h1 "Welcome, " ,band-name " (Band)!")
         (h2 "Your Concert Listings")
         (ul ,@(map (lambda (ic)
                      (let ([idx (car ic)]
                            [c (cdr ic)])
                        `(li ,(string-append (second c) " - " (third c) (if (fifth c) "" " (Not Available)"))
                             " " (a ([href ,(embed/url (lambda (req) (edit-listing-page req idx user)))]) "[Edit]")
                             " " (a ([href ,(embed/url (lambda (req) (delete-listing-handler req idx user)))]) "[Delete]"))))
                    my-indexed-listings))
         (h3 "Create New Listing")
         (form ([action ,(embed/url (lambda (req) (create-listing-handler req user)))] [method "post"])
           "Title: " (input ([type "text"] [name "title"])) (br)
           "Date: " (input ([type "text"] [name "date"] [placeholder "YYYY-MM-DD"])) (br)
           "Price: " (input ([type "text"] [name "price"])) (br)
           "Venue: " (input ([type "text"] [name "venue"])) (br)
           "Genre: " (input ([type "text"] [name "genre"])) (br)
           (input ([type "submit"] [value "Create Listing"])))
         (a ([href ,(embed/url (lambda (req) (start req)))]) "Logout")))))))

(define (create-listing-handler request user)
  (define band-email (first user))
  (define title (extract-binding/default 'title (request-bindings request) ""))
  (define date (extract-binding/default 'date (request-bindings request) ""))
  (define price (extract-binding/default 'price (request-bindings request) ""))
  (define venue (extract-binding/default 'venue (request-bindings request) ""))
  (define genre (extract-binding/default 'genre (request-bindings request) ""))
  (when (not (or (string-blank? title) (string-blank? date) (string-blank? price) (string-blank? venue) (string-blank? genre)))
    (add-concert! band-email title date price venue genre))
  (band-dashboard user))

(define (edit-listing-page request idx user)
  (define concert (get-concert idx))
  (define band-email (first user))
  (if (or (not concert) (not (string=? (first concert) band-email)))
      (band-dashboard user)
      (let ([title (second concert)]
            [date (third concert)]
            [price (fourth concert)]
            [available? (fifth concert)]
            [venue (sixth concert)]
            [genre (seventh concert)])
        (send/suspend/dispatch
         (lambda (embed/url)
           (response/xexpr
            `(html
              (head (title "Edit Listing - " ,title))
              (body
               (h1 "Edit Your Listing")
               (form ([action ,(embed/url (lambda (req) (update-listing-handler req idx user)))] [method "post"])
                 "Title: " (input ([type "text"] [name "title"] [value ,title])) (br)
                 "Date: " (input ([type "text"] [name "date"] [value ,date])) (br)
                 "Price: " (input ([type "text"] [name "price"] [value ,price])) (br)
                 "Venue: " (input ([type "text"] [name "venue"] [value ,venue])) (br)
                 "Genre: " (input ([type "text"] [name "genre"] [value ,genre])) (br)
                 (input ([type "checkbox"] [name "available"] [value "true"] ,@(if available? '((checked "checked")) '()))) " Tickets Available" (br)
                 (input ([type "submit"] [value "Update Listing"])))
               (a ([href ,(embed/url (lambda (req) (band-dashboard user)))]) "Back to Dashboard")))))))))

(define (update-listing-handler request idx user)
  (define concert (get-concert idx))
  (define band-email (first user))
  (when (and concert (string=? (first concert) band-email))
    (let* ([bindings (request-bindings request)]
           [title (extract-binding/default 'title bindings (second concert))]
           [date (extract-binding/default 'date bindings (third concert))]
           [price (extract-binding/default 'price bindings (fourth concert))]
           [venue (extract-binding/default 'venue bindings (sixth concert))]
           [genre (extract-binding/default 'genre bindings (seventh concert))]
           [available (let ([avail-binding (assoc 'available bindings)])
                        (and avail-binding (string=? (cdr avail-binding) "true")))])
      (when (not (or (string-blank? title) (string-blank? date) (string-blank? price)))
        (update-concert! idx title date price available venue genre))))
  (band-dashboard user))

(define (delete-listing-handler request idx user)
  (define concert (get-concert idx))
  (define band-email (first user))
  (when (and concert (string=? (first concert) band-email))
    (delete-concert! idx))
  (band-dashboard user))

(define (select-ticket-page request idx user)
  (define concert (get-concert idx))
  (if (not concert)
      (user-dashboard user #f)
      (let ([title (second concert)]
            [date (third concert)])
        (send/suspend/dispatch
         (lambda (embed/url)
           (response/xexpr
            `(html
              (head (title "Select Ticket - " ,title))
              (body
               (h1 "Select Ticket Type")
               (p (strong "Event: ") ,title)
               (p (strong "Date: ") ,date)
               (form ([action ,(embed/url (lambda (req) (ticket-details-page req idx user)))] [method "post"])
                 (p "Choose your ticket type:")
                 ,@(map (lambda (tt)
                          `(div
                            (input ([type "radio"] [name "ticket-type"] [value ,(first tt)] [required "required"]))
                            " " ,(first tt) " - $" ,(second tt)))
                        ticket-types)
                 (br)
                 (input ([type "submit"] [value "Continue"]))
               )
               (a ([href ,(embed/url (lambda (req) (user-dashboard user #f)))]) "Back to Dashboard")))))))))

(define (ticket-details-page request idx user)
  (define concert (get-concert idx))
  (if (not concert)
      (user-dashboard user #f)
      (let* ([title (second concert)]
             [date (third concert)]
             [ticket-type (extract-binding/default 'ticket-type (request-bindings request) "Standard")]
             [ticket-price-str (let ([tt (findf (lambda (tt) (string=? (first tt) ticket-type)) ticket-types)])
                                 (if tt (second tt) "0"))])
        (send/suspend/dispatch
         (lambda (embed/url)
           (response/xexpr
            `(html
              (head (title "Enter Details - " ,title))
              (body
               (h1 "Enter Your Details")
               (p (strong "Event: ") ,title " (" ,ticket-type ")")
               (p (strong "Price: ") "$" ,ticket-price-str)
               (form ([action ,(embed/url (lambda (req) (payment-page req idx ticket-type user)))] [method "post"])
                 "Name: " (input ([type "text"] [name "buyer-name"])) (br)
                 "DOB: " (input ([type "text"] [name "buyer-dob"])) (br)
                 "Phone: " (input ([type "text"] [name "buyer-phone"])) (br)
                 "Address: " (input ([type "text"] [name "buyer-address"])) (br)
                 "Email: " (input ([type "email"] [name "buyer-email"])) (br)
                 (input ([type "submit"] [value "Confirm Purchase"]))
               )
               (a ([href ,(embed/url (lambda (req) (select-ticket-page req idx user)))]) "Back")))))))))

(define (payment-page request idx ticket-type user)
  (define concert (get-concert idx))
  (if (not concert)
      (user-dashboard user #f)
      (let ([buyer-name (extract-binding/default 'buyer-name (request-bindings request) "")]
            [buyer-dob (extract-binding/default 'buyer-dob (request-bindings request) "")]
            [buyer-phone (extract-binding/default 'buyer-phone (request-bindings request) "")]
            [buyer-address (extract-binding/default 'buyer-address (request-bindings request) "")]
            [buyer-email (extract-binding/default 'buyer-email (request-bindings request) "")])
        (add-ticket! idx ticket-type buyer-name buyer-dob buyer-phone buyer-address buyer-email)
        (send/suspend/dispatch
         (lambda (embed/url)
           (response/xexpr
            `(html
              (head (title "Purchase Confirmed"))
              (body
               (h1 "Purchase Confirmed!")
               (p "Thank you, " ,(third user) ". Your ticket has been purchased.")
               (a ([href ,(embed/url (lambda (req) (user-dashboard user #f)))]) "Return to Dashboard")))))))))
