#lang web-server/insta

(require web-server/servlet-env)

(define (string-blank? s)
  (or (string=? s "") (regexp-match? #px"^\\s*$" s)))

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
   (list "band@example.com" "Rockin' the Night Away" "2025-08-01" "50")
   (list "band@example.com" "An Evening of Jazz" "2025-08-15" "40")
   (list "band@example.com" "Summer Pop Festival" "2025-09-10" "60")))


(define tickets '())
(define next-ticket-id 1001)

(define ticket-types
  (list
   (list "Standard" "50")
   (list "VIP" "100")
   (list "Student" "30")))

(define (add-user! email password name type)
  (set! users (cons (list email password name type) users)))

(define (add-concert! band-email title date price)
  (set! concerts (cons (list band-email title date price) concerts)))

(define (find-user email)
  (findf (lambda (u) (string=? (first u) email)) users))

(define (band-concerts band-email)
  (filter (lambda (c) (string=? (first c) band-email)) concerts))

(define (get-concert idx)
  (if (and (integer? idx) (>= idx 0) (< idx (length concerts)))
      (list-ref concerts idx)
      #f))

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
          (user-dashboard user))
      (start request)))

(define (user-dashboard user)
  (define user-name (third user))
  (define indexed-concerts (list->indexed-list concerts))
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (head (title "Tick3tM4s3r - Dashboard"))
        (body
         (h1 "Welcome, " ,user-name "!")
         (h2 "Available Concerts")
         (ul
           ,@(map (lambda (indexed-concert)
                    (define idx (car indexed-concert))
                    (define concert (cdr indexed-concert))
                    `(li (a ([href ,(embed/url (lambda (req) (select-ticket-page req idx user)))])
                          ,(string-append (second concert) " - " (third concert) " - $" (fourth concert)))))
                  indexed-concerts)))
         (a ([href ,(embed/url (lambda (req) (start req)))]) "Logout"))))))

(define (band-dashboard user)
  (define band-email (first user))
  (define band-name (third user))
  (define my-listings (band-concerts band-email))
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (head (title "Tick3tM4s3r - Band Dashboard"))
        (body
         (h1 "Welcome, " ,band-name " (Band)!")
         (h2 "Your Concert Listings")
         (ul ,@(map (lambda (c) `(li ,(string-append (second c) " - " (third c)))) my-listings))
         (h3 "Create New Listing")
         (form ([action ,(embed/url (lambda (req) (create-listing-handler req user)))] [method "post"])
           "Title: " (input ([type "text"] [name "title"])) (br)
           "Date: " (input ([type "text"] [name "date"])) (br)
           "Price: " (input ([type "text"] [name "price"])) (br)
           (input ([type "submit"] [value "Create Listing"])))
         (a ([href ,(embed/url (lambda (req) (start req)))]) "Logout")))))))

(define (create-listing-handler request user)
  (define band-email (first user))
  (define title (extract-binding/default 'title (request-bindings request) ""))
  (define date (extract-binding/default 'date (request-bindings request) ""))
  (define price (extract-binding/default 'price (request-bindings request) ""))
  (when (not (or (string-blank? title) (string-blank? date) (string-blank? price)))
    (add-concert! band-email title date price))
  (band-dashboard user))

(define (select-ticket-page request idx user)
  (define concert (get-concert idx))
  (if (not concert)
      (user-dashboard user)
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
               (a ([href ,(embed/url (lambda (req) (user-dashboard user)))]) "Back to Dashboard")))))))))

(define (ticket-details-page request idx user)
  (define concert (get-concert idx))
  (if (not concert)
      (user-dashboard user)
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
      (user-dashboard user)
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
               (a ([href ,(embed/url (lambda (req) (user-dashboard user)))]) "Return to Dashboard")))))))))
