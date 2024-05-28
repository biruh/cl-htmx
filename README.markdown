# Cl-HTMX

Common lisp macros to enable web development using [HTMX](https://htmx.org/) 

## Goal

* Simplify web development using common lisp
* Opinionated design choices, while offering options for customization.
* Leverage existing libraries


## Usage

The project is not yet submitted to quicklisp repository. It will be submitted onces the API matures.

To use the project, check out the repository to your quicklisp local directory and do

`
ql:quickload :cl-htmx


## API Design (DRAFT)


```lisp
(def-api "/contact/1/edit" (lambda (request)
    (html
        (swap
            (form 
                (div :class "form-group" 
                    (label "First Name") 
                    (input :type "text" :name "firstname" :value "Joe")) 
                (div :class "form-group" 
                    (label "Last Name") 
                    (input :type "text" :name "lastname" :value "Blow"))
                (div :class "form-group" 
                    (label "Email") 
                    (input :type "email" :name "email" :value "joe@blow.com"))
                (button :class "btn" "Submit")
                (button :class "btn" :on-click (swap "/contact/1") "Cancel")
            )))))

(def-api "/contact/1" (lambda (request))
    (html
        (swap
            (div 
                (div (label "First Name:") "Joe")
                (div (label "Last Name:") "Blow")
                (div (label "Email:") "joe@blow.com")
                (button :class "btn btn-primary" 
                        :on-click (swap "/contact/1/edit") "Click To Edit")))))
```
will translate to 
```html
<!-- returned from /contact/1  -->
<div hx-target="this" hx-swap="outerHTML">
    <div><label>First Name</label>: Joe</div>
    <div><label>Last Name</label>: Blow</div>
    <div><label>Email</label>: joe@blow.com</div>
    <button hx-get="/contact/1/edit" class="btn btn-primary">
    Click To Edit
    </button>
</div>

<!-- returned from /contact/1/edit -->
<form hx-put="/contact/1" hx-target="this" hx-swap="outerHTML">
  <div class="form-group">
    <label>First Name</label>
    <input type="text" name="firstName" value="Joe">
  </div>
  <div class="form-group">
    <label>Last Name</label>
    <input type="text" name="lastName" value="Blow">
  </div>
  <div class="form-group">
    <label>Email Address</label>
    <input type="email" name="email" value="joe@blow.com">
  </div>
  <button class="btn">Submit</button>
  <button class="btn" hx-get="/contact/1">Cancel</button>
</form>
```


### HTML Generation
We use [spinnerate](https://github.com/ruricolist/spinneret) to convert s-exp to HTML.


### Web Server
We use [Hunchentoot](https://edicl.github.io/hunchentoot) to create API end-points and serve files. 


### Client Side Scripting



## Author

* Biruh

## Copyright

Copyright (c) 2023 Biruh Mekonnen (biruh dot t at gmail.com)

## License

Licensed under the MIT License.
