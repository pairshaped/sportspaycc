# Sportspay Credit Card Widget


## Overview

Embeddable javscript widget to ask for Credit Card information and submit it directly to SportsPay.

## Using the widget

### 1. Include the Widget's Javascript

You'll need to include the widget's javascript.
```<script src="https://pairshaped.github.io/sportspaycc/prod.min.js"></script>```

### 2. Add the placeholder div to your page where you want the widget to be inserted.

```<div id="sportspaycc"></div>```

It's important that the ID here is the same used in the next step.

### 3. Configure and Load the Widget

The widget is embedded using Javascript, which is how web browsers can enhance a site's functionality.

```
<script>
  var sportspaycc = Elm.SportspayCC.init(
    {
      node: document.getElementById("sportspaycc"), // Must be the same as the div id above.
      flags: {
        sportspayHost: "replaceme", // Host comes from Sportspay API call.
        sportspayApiKey: "replaceme", // API key comes from Sportspay API call.
        transactionAmount: "$100.00", // For display purposes only, but should match your total sale with tax.
        transactionUrl: "replaceme" // This is where to submit the resulting one time token in order to complete the payment.
      }
    }
  )
</script>
```


## For Contributors

### Installing Dependencies

We use elm and elm-live for development. You can install these via npm.

```
npm install
```

### Running It

Edit dev.html and configure the application's parameters for your environment. Then run it:

```
npm start
```

### Production Deployment

Make sure you have uglify-js installed to compress the production js.
```
npm install -g uglify-js
```

Compile and optimize for production using:

```
./prod.sh
```

### Third Party Libraries

We are using the following third party libraries:

- https://github.com/abadi199/elm-creditcard
- https://github.com/NoRedInk/elm-json-decode-pipeline


## Copyright and License

[See LICENSE.md](LICENSE.md)
