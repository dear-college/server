// Import our custom CSS
import '../css/styles.scss'

// Import all of Bootstrap's JS
import * as bootstrap from 'bootstrap'

import 'htmx.org';

document.addEventListener('htmx:afterRequest', e => {
  if (!e.detail.xhr.status.toString().startsWith('2')) {
    let message = e.detail.xhr.responseText;
    let type = 'danger';
    
    const alertPlaceholder = document.getElementById('alerts');

    const wrapper = document.createElement('div');
    wrapper.innerHTML = [
      `<div class="alert alert-${type} alert-dismissible" role="alert">`,
      `   <div>${message}</div>`,
      '   <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>',
      '</div>'
    ].join('');
    alertPlaceholder.append(wrapper);
  }
});
