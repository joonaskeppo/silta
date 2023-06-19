// ---- base ---

// Silta-specific HTML element attributes
const specialAttrs = {
    events: 'silta-events',
    viewId: 'silta-view-id',
    viewName: 'silta-view-name',
    viewType: 'silta-view-type'
}

const eventsSelector =  '[' + specialAttrs.events + ']';

const clientId = self.crypto.randomUUID();

// event handlers are provided as `silta-events` (map of 2d arrays)
function findElementsWithEvents(nodes) {
    return nodes.reduce((acc, node) => {
        if (node.hasAttribute(specialAttrs.events)) {
            acc.push(node);
        }

        node.querySelectorAll(eventsSelector).forEach((elt) => {
            acc.push(elt);
        });

        return acc;
    }, []);
}

// return array of nodes matching `nodeOrSelector`.
// returns array containing only `nodeOrSelector` if HTMLElement.
function resolveNodes(nodeOrSelector) {
    return (nodeOrSelector instanceof HTMLElement) ?
        [nodeOrSelector] :
        Array.from(document.querySelectorAll(nodeOrSelector));
}

function makeQueryString(params) {
    return '?' + Object.keys(params).map(k => k + '=' + params[k]).join('&');
}

function makeRequestUrl(endpoint, params) {
    return endpoint + makeQueryString({"__params": JSON.stringify(params)});
}

function parseEvent(event, viewId) {
    const thisViewSelector = `[silta-view-id="${viewId}"]`;
    const withoutConfig = Array.isArray(event[1]);
    let type, config, endpoint;

    type = event[0];

    config = withoutConfig ?
        { target: viewId ? thisViewSelector : undefined } :
        {
            ... event[1],
            target: event[1].target ?
                event[1].target :
                (viewId ? thisViewSelector : undefined)
        };

    endpoint = withoutConfig ? event[1] : event[2];
    endpoint = endpoint &&
        {
            method: 'GET',
            url: makeRequestUrl(endpoint[0], endpoint.slice(1))
        };

    return { type, config, endpoint };
}

function setupEventHandlers(nodes) {
    nodes.forEach((node) => {
        const stringifiedEvents = node.getAttribute(specialAttrs.events);
        if (!stringifiedEvents) {
            console.log("No events for node, skipping:", node)
        }
        if (stringifiedEvents) {
            try {
                const events = JSON.parse(stringifiedEvents);
                const viewId = node.getAttribute(specialAttrs.viewId);

                Object.keys(events).forEach((eventType) => {
                    let callbacks = [];

                    events[eventType].forEach((event) => {
                        const { type, config, endpoint } = parseEvent(event, viewId);

                        if (config && !config.target) {
                            console.error('Failed to setup event -- could not infer target:', { node, descriptor, config });
                        }

                        console.log(`Adding '${eventType}' type listener for node:`, node);

                        if (!endpoint) {
                            // descriptor does not contain 'endpoint' description -> `null` resultant HTML
                            callbacks.push(() => handleUpdate(type, config)(null));
                        } else {
                            const handler = handleUpdate(type, config);

                            console.log(`Initializing '${type}' action with endpoint:`, config, endpoint, handler);

                            callbacks.push(() => handleAjaxRequest(endpoint, handler));
                        }
                    });

                    // attach all callbacks to single event listener of given type
                    node.addEventListener(eventType, () => {
                        console.log(`Calling ${callbacks.length} '${eventType}' callbacks for node:`, node);
                        callbacks.forEach((cb) => cb());
                    });
                });
            } catch (error) {
                console.error("Failed to parse events for node:", node);
                console.error(error);
            }
        }
    });
}

function handleAjaxRequest(req, handler) {
    let xhr = new XMLHttpRequest();

    // response handling
    xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            handler(xhr.responseText);
        }
    }
    // send request
    xhr.open(req.method, req.url);
    xhr.setRequestHeader('client-id', clientId);
    xhr.send();
}

// --- handlers ---

const createElement = (html) => {
    let newElt = document.createElement('div');
    newElt.innerHTML = html;
    return newElt.firstChild;
}

const elementUpdates = {
    swap: (html, elt) => {
        const newElt = createElement(html);
        elt.replaceWith(newElt);
        return [newElt];
    },
    append: (html, elt) => {
        const newElt = createElement(html);
        elt.append(newElt);
        return [newElt];
    },
    prepend: (html, elt) => {
        const newElt = createElement(html);
        elt.prepend(newElt);
        return [newElt];
    },
    remove: (_, elt) => {
        elt.remove();
        return [];
    }
}

function handleUpdate(type, { target }) {
    const updateFn = elementUpdates[type];

    return function(html) {
        let nodes = [];
        let createdNodes = [];

        // invoke handler for each DOM element matching `selector`
        document.querySelectorAll(target).forEach((elt) => {
            nodes = updateFn(html, elt);
            createdNodes = createdNodes.concat(nodes);
        })

        // refresh event handlers
        console.log("Refreshing event handlers for:", createdNodes);
        setupEventHandlers(createdNodes);
    }
}

// Initialize with root-most node, <body>
setupEventHandlers(findElementsWithEvents(resolveNodes('body')));
