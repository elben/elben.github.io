var Clustering=function(){this.data=[];this.cc=[];this.clusters=[];this.cluster=function(e){this.clusters=[];this.gc(e);for(var t=0;t<20;t++){this.fc();this.mv()}};this.fc=function(){if(this.cc.length==0){return[]}this.clusters=Array(this.cc.length);for(var e=0;e<this.data.length;++e){var t=this.data[e];var n=this.fcc(t);if(!clusters[n]){clusters[n]=[]}clusters[n].push(t)}return clusters};this.mv=function(){if(this.clusters.length==0){return}var e=[];for(var t in this.clusters){var n=this.clusters[t];e.push(this.m(n))}this.cc=e};this.fcc=function(e){if(this.cc.length==0){return undefined}var t;var n=Infinity;for(var r=0;r<this.cc.length;++r){var i=this.cc[r];var s=this.euclidDistance(e,i);if(s<n){n=s;t=r}}return t};this.m=function(e){if(e.length===0){return null}var t=0;var n=0;for(var r in e){t+=e[r][0];n+=e[r][1]}return[parseInt(t/e.length),parseInt(n/e.length)]};this.euclidDistance=function(e,t){return Math.sqrt(Math.pow(e[0]-t[0],2)+Math.pow(e[1]-t[1],2))};this.gc=function(e){this.cc=[];for(var t=0;t<parseInt(e);++t){x=this.randomInt(0,600);y=this.randomInt(0,400);this.cc.push([x,y])}};this.importData=function(e){this.data=[];for(var t in e){this.data.push([parseInt(e[t][0]),parseInt(e[t][1])])}};this.randomInt=function(e,t){var n=t-e;return e+Math.floor(Math.random()*n)};return this}
