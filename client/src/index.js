import packageInfo from '../package.json';
import * as jose from 'jose';

console.log(`dear.college: version ${packageInfo.version}`);

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
  const data = encoder.encode(text);
  console.log("hashing",text);
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  return bufferToHex(hashBuffer);
}

export async function getProgress( location = undefined ) {
  if (location === undefined) location = window.location.href;

  let jwt = localStorage.getItem("dear.college/jwt");
  let claims = jose.decodeJwt(jwt);
  console.log(claims);

  let sha = await hashHex(location);
  
  let aud = claims.aud;
  
  // TODO: be more careful appending the path to the audience
  const url = `${aud}api/v1/progress/${sha}`;
  
  console.log(url);

  // Make the fetch request
  try {
    const response = await fetch(url, {
      method: 'GET',
      credentials: 'include',
      headers: {
        'Authorization': `Bearer ${jwt}`,
        'X-Worksheet': location
      }
    });

    // Check if the response is OK and return the body
    if (response.ok) {
      return await response.json(); // Assuming the response is JSON
    } else {
      throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
    }
  } catch (error) {
    console.error('Fetch error:', error);
    throw error;
  }
}
