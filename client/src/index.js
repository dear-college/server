import packageInfo from '../package.json';
import * as jose from 'jose';
import * as JWP from 'json-work-proof';

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

export async function fetchWithJwt( location, url, params ) {
  let jwt = localStorage.getItem("dear.college/jwt");
  if (jwt === null) throw new Error('not logged in');

  let claims = jose.decodeJwt(jwt);
  let sha = await hashHex(location);
  let aud = claims.aud;

  const relativeUrl = url + `/${sha}`;
  const absoluteUrl = new URL(relativeUrl, aud);

  try {
    params.headers['Authorization'] = `Bearer ${jwt}`;
    params.credentials = 'include';
    params.headers['Worksheet'] = location;

    if (params.headers['JSON-Work-Proof']) {
      const jwp = new JWP(16);
      const token = await jwp.generate({ sub: relativeUrl });
      params.headers['JSON-Work-Proof'] = token;
    }
    return await fetch(absoluteUrl, params);
  } catch (error) {
    console.error('Fetch error:', error);
    throw error;
  }
}

export async function getProgress( location = undefined ) {
  if (location === undefined) location = window.location.href;

  const response = await fetchWithJwt( location, `/api/v1/progress`, {
    method: 'GET',
    headers: {
      'Accept': 'application/json'
    }
  });

  if (response.ok) {
    let result = await response.json();
    return result.hasOwnProperty('progress') ? parseFloat(result.progress) : NaN;
  } else {
    throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
  }
}

export async function putProgress( progress = 1.0, location = undefined ) {
  if (location === undefined) location = window.location.href;

  const response = await fetchWithJwt( location, `/api/v1/progress`, {
    method: 'PUT',
    headers: {
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
}

export async function getState( location = undefined ) {
  if (location === undefined) location = window.location.href;

  const response = await fetchWithJwt( location, `/api/v1/state`, {
    method: 'GET',
    headers: {
      'Accept': 'application/json'
    }
  });

  if (response.ok) {
    return await response.json();
  } else {
    throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
  }
}

export async function putState( state, location = undefined ) {
  if (location === undefined) location = window.location.href;

  const response = await fetchWithJwt( location, `/api/v1/state`, {
    method: 'PUT',
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json',
      'JSON-Work-Proof': true
    },
    body: JSON.stringify(state)
  });

  if (response.ok) {
    return;
  } else {
    throw new Error(`Failed to fetch: ${response.status} ${response.statusText}`);
  }
}
