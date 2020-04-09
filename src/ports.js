export function backupToLocalStorage(ports, data) {
  localStorage.setItem("_the_hero_journey_", data);
}

export function restoreFromLocalStorage(ports) {
  // console.log(localStorage.getItem("_the_hero_journey_"));
}
