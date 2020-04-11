export function backupToLocalStorage(ports, data) {
  localStorage.setItem("_the_hero_journey_", data);
}

export function restoreFromLocalStorage(ports) {
  // console.log(localStorage.getItem("_the_hero_journey_"));
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
