import packageInfo from '../package.json';
import * as jose from 'jose';

console.log(`@dear.college/client version ${packageInfo.version}`);

function removeHash () { 
  history.pushState("", document.title, window.location.pathname + window.location.search);
}

export function logout() {
  localStorage.removeItem("dear.college/jwt");
}

export function login() {
  let jwt = window.location.hash.substring(1);
  let claims = {};

  try {
    claims = jose.decodeJwt(jwt);
  } catch (error) {
    console.log(error);
    return;
  }

  removeHash();
  localStorage.setItem("dear.college/jwt", jwt);
}

function bufferToHex(buffer) {
  return Array.from(new Uint8Array(buffer))
    .map(b => b.toString(16).padStart(2, '0'))
    .join('');
}

async function hashHex(text) {
  const encoder = new TextEncoder();
  return bufferToHex(await crypto.subtle.digest('SHA-256', encoder.encode(text)));
}

export async function getProgress( location = undefined ) {
  if (location === undefined) location = window.location.href;

  let jwt = localStorage.getItem("dear.college/jwt");
  if (jwt === null) throw new Error('not logged in');
  
  let claims = jose.decodeJwt(jwt);
  let sha = await hashHex(location);
  let aud = claims.aud;

  const url = new URL(`/api/v1/progress/${sha}`, aud);

  try {
    const response = await fetch(url, {
      method: 'GET',
      credentials: 'include',
      headers: {
        'Authorization': `Bearer ${jwt}`,
        'X-Worksheet': location,
        'Accept': 'application/json'
      }
    });

    if (response.ok) {
      let result = await response.json();
      return result.hasOwnProperty('progress') ? parseFloat(result.progress) : NaN;
    } else {
      throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
    }
  } catch (error) {
    console.error('Fetch error:', error);
    throw error;
  }
}

export async function putProgress( progress = 1.0, location = undefined ) {
  if (location === undefined) location = window.location.href;

  let jwt = localStorage.getItem("dear.college/jwt");
  if (jwt === null) throw new Error('not logged in');
  
  let claims = jose.decodeJwt(jwt);
  let sha = await hashHex(location);
  let aud = claims.aud;

  const url = new URL(`/api/v1/progress/${sha}`, aud);
  
  try {
    const response = await fetch(url, {
      method: 'PUT',
      credentials: 'include',
      headers: {
        'Authorization': `Bearer ${jwt}`,
        'X-Worksheet': location,
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({progress: progress})
    });

    if (response.ok) {
      return;
    } else {
      throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
    }
  } catch (error) {
    console.error('Fetch error:', error);
    throw error;
  }
}
