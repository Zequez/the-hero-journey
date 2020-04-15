export function backupToLocalStorage(ports, data) {
  localStorage.setItem("_the_hero_journey_", data);
}

export function restoreFromLocalStorage(ports) {
  // console.log(localStorage.getItem("_the_hero_journey_"));
}

export function downloadBackup() {
  let data = localStorage.getItem("_the_hero_journey_");
  let j = document.createElement("a");
  j.id = "download";
  j.download = "the-hero-journey-" + Date.now() + ".json";
  j.href = URL.createObjectURL(new Blob([data]));
  j.click();
  setTimeout(() => window.URL.revokeObjectURL(j.href), 1000);
}

// On the initial render the scroll doesn't work very well, so we hack it out.
let scrollTimeout = null;
export function scrollViewportTo(ports, scrollPos, timeout = 100) {
  let vp = document.getElementById("viewport");
  clearTimeout(scrollTimeout);
  // console.log("Scrollheight: ", vp.scrollHeight);
  if (vp.scrollTop !== scrollPos) {
    vp.scroll(0, scrollPos);
    scrollTimeout = setTimeout(
      () => scrollViewportTo(ports, scrollPos, timeout - 10),
      10
    );
  }
}

// export function signUpWithEmail(ports, email) {
//   let provider = new firebase.auth.GoogleAuthProvider();

//   // var actionCodeSettings = {
//   //   // URL you want to redirect back to. The domain (www.example.com) for this
//   //   // URL must be whitelisted in the Firebase Console.
//   //   url: "http://localhost:3000",
//   //   // This must be true.
//   //   handleCodeInApp: true,
//   // };

//   // firebase
//   //   .auth()
//   //   .sendSignInLinkToEmail(email, actionCodeSettings)
//   //   .then(function () {
//   //     // The link was successfully sent. Inform the user.
//   //     // Save the email locally so you don't need to ask the user for it again
//   //     // if they open the link on the same device.
//   //   })
//   //   .catch(function (error) {
//   //     console.log("Firebase error", error);
//   //     // Some error occurred, you can inspect the code: error.code
//   //   });
// }

// export function getAuthStatus(ports) {
//   // if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
//   //   firebase.auth().signInWithEmailLink(email, window.location.href)
//   //   .then(function(result) {
//   //     // Clear email from storage.
//   //     window.localStorage.removeItem('emailForSignIn');
//   //     // You can access the new user via result.user
//   //     // Additional user info profile not available via:
//   //     // result.additionalUserInfo.profile == null
//   //     // You can check if the user is new or existing:
//   //     // result.additionalUserInfo.isNewUser
//   //     ports.receiveAuthStatus.send(result.user);
//   //   })
//   //   .catch(function(error) {
//   //     // Some error occurred, you can inspect the code: error.code
//   //     // Common errors could be invalid email and invalid or expired OTPs
//   //     console.log("Sign in error", error);
//   //     ports.receiveAuthStatus.send(null);
//   //   });
//   // } else {
//   //   ports.receiveAuthStatus.send(null);
//   // }
// }
