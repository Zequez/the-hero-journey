import "./Main.css";
import { Elm } from "./Main.elm";
import * as ports from "./ports.js";
import * as firebase from "firebase/app";
import "firebase/auth";
import "firebase/firestore";
import * as ElmDebugger from "elm-debug-transformer";
// import * as serviceWorker from "./serviceWorker";

console.log("ENV defined: ", process.env.ELM_APP_API_KEY !== undefined);

ElmDebugger.register();

const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem("_the_hero_journey_") || null),
  node: document.getElementById("root"),
});

// ELM_APP_* variables are automatically taken from .env by create-elm-app
// https://github.com/halfzebra/create-elm-app/blob/master/template/README.md#using-custom-environment-variables

const firebaseConfig = {
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  databaseURL: process.env.ELM_APP_DATABASE_URL,
  projectId: process.env.ELM_APP_PROJECT_ID,
  storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
  messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
  appId: process.env.ELM_APP_APP_ID,
};

firebase.initializeApp(firebaseConfig);

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();

Object.keys(ports).forEach((port) => {
  if (app.ports[port]) {
    app.ports[port].subscribe((data) => {
      ports[port](app.ports, data);
    });
  }
});

app.ports.signIn.subscribe(() => {
  console.log("LogIn called");
  firebase
    .auth()
    .signInWithPopup(provider)
    .then((result) => {
      result.user.getIdToken().then((idToken) => {
        app.ports.signInInfo.send({
          token: idToken,
          email: result.user.email,
          uid: result.user.uid,
        });
      });
    })
    .catch((error) => {
      app.ports.signInError.send({
        code: error.code,
        message: error.message,
      });
    });
});

app.ports.signOut.subscribe(() => {
  console.log("LogOut called");
  firebase.auth().signOut();
});

//  Observer on user info
firebase.auth().onAuthStateChanged((user) => {
  console.log("called");
  if (user) {
    user
      .getIdToken()
      .then((idToken) => {
        app.ports.signInInfo.send({
          token: idToken,
          email: user.email,
          uid: user.uid,
        });
      })
      .catch((error) => {
        console.log("Error when retrieving cached user");
        console.log(error);
      });

    // Set up listened on new messages
    db.collection(`users/${user.uid}/logs`).onSnapshot((docs) => {
      console.log("Received new snapshot");
      const logs = [];

      docs.forEach((doc) => {
        if (doc.data().content) {
          logs.push(doc.data().content);
        }
      });

      app.ports.receiveLogs.send({
        logs: logs,
      });
    });
  }
});

app.ports.saveLog.subscribe((data) => {
  console.log(`saving log to database : ${data.content}`);

  db.collection(`users/${data.uid}/logs`)
    .add({
      content: data.content,
    })
    .catch((error) => {
      app.ports.signInError.send({
        code: error.code,
        message: error.message,
      });
    });
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
