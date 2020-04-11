import "./Main.css";
import { Elm } from "./Main.elm";
import * as ports from "./ports.js";
// import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem("_the_hero_journey_") || "{}"),
  node: document.getElementById("root"),
});

Object.keys(ports).forEach((port) => {
  if (app.ports[port]) {
    app.ports[port].subscribe((data) => {
      ports[port](app.ports, data);
    });
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
