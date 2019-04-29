import { Elm } from './src/Main.elm'

Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    seed: window.location.hash ? window.location.hash.slice(1) : null
  }
})
