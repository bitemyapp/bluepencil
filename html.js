import {stateToHTML} from 'draft-js-export-html';

let contentState = {"getBlocksAsArray": function() { return ""; }};

let html = stateToHTML(contentState);

console.log(html);
