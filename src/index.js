import { Elm } from "./Main.elm";

// read data from localStorage for initial load
var storedData = localStorage.getItem('todos-list');
var flags = storedData ? JSON.parse(storedData) : null;

// tell the elm app to apply the view to this html node
var node = document.getElementById("root");

// start up the elm app
var app = Elm.Main.init({ flags, node });

// when the elm app invokes the setStorage port, call this function to actually store the data
app.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('todos-list', JSON.stringify(state));
});