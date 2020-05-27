let symbols = document.getElementsByClassName('symbol');

function searchSymbol(filter) {
    // Function for searching symbols in package
    for (let i = 0; i < symbols.length; i++) {
        let name = symbols[i].childNodes[3].childNodes[0].innerHTML;
        if (name.toUpperCase().indexOf(filter.toUpperCase()) > -1) {
          symbols[i].style.display = '';
        } else {
          symbols[i].style.display = 'none';
        }
    }
}

function toggleAttribute(attribute, value) {
    // Function for managing url attributes
    let items = window.location.search.substring(1).split('&');
    let url = window.location.pathname;

    let toggled = false;
    let mark = false; // question mark present?

    for (let i = 0; i < items.length; i++) {
        let s = items[i].split('=');
        if (s.length === 2) {
            if (s[1] !== value || s[0] !== attribute) {
                if (!mark) {
                    url += '?';
                    mark = true;
                } else {
                    url += '&';
                }
                url += s[0] + '=' + s[1];
            } else {
                toggled = true;
            }
        }
    }

    if (!toggled) {
        if (!mark) {
            url += '?';
        } else {
            url += '&';
        }
        url += attribute + '=' + value;
    }

    window.history.replaceState(null, null, url);
}

let lastSystem;
let lastPackage;

function hideAll(className) {
    let items = document.getElementsByClassName(className);

    for (let i = 0; i < items.length; i++) {
        items[i].style.display = 'none'
    }
}

function showSelected(systemName, packageName) {
    hideAll('system');
    hideAll('package');
    document.getElementById('current-system').innerHTML = systemName;
    document.getElementById('current-package').innerHTML = packageName;
    document.getElementById('system-description').innerHTML = document.getElementById('system-'+systemName+'-description').innerText;
    document.getElementById('package-description').innerHTML = document.getElementById('package-'+packageName+'-description').innerText;
    document.getElementById('system-'+systemName).style.display = '';
    document.getElementById('package-'+packageName).style.display = '';

    if (document.getElementById('package-description').innerHTML === '') {
        document.getElementById('package-description').style.display = 'none';
        document.getElementById('package-description-title').style.display = 'none';
    } else {
        document.getElementById('package-description').style.display = '';
        document.getElementById('package-description-title').style.display = '';
    }
    if (lastSystem) toggleAttribute('system', lastSystem);
    if (lastPackage) toggleAttribute('package', lastPackage);
    toggleAttribute('system', systemName);
    toggleAttribute('package', packageName);

    lastSystem = systemName;
    lastPackage = packageName;
}

function parseAttributes() {
    // Function for parting url attributes on load
    let items = window.location.search.substring(1).split('&');

    let systemName;
    let packageName;

    for (let i = 0; i < items.length; i++) {
        let s = items[i].split('=');
        if (s.length === 2 && s[0] === 'find') {
            searchSymbol(s[1]);
        }
        if (s.length === 2 && s[0] === 'system') {
            systemName = s[1];
            lastSystem = s[1];
        }
        if (s.length === 2 && s[0] === 'package') {
            packageName = s[1];
            lastPackage = s[1];
        }
    }

    let select = document.getElementById('select');

    if (select) {
        if (systemName && packageName) {
            showSelected(systemName, packageName);
            select.value = systemName + '.' + packageName;
        } else {
            select.oninput(undefined);
        }
    }
}
