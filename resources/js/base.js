// ---- base ---

// Silta-specific HTML element attributes
const specialAttrs = {
    events: 'silta-events',
    viewId: 'silta-view-id',
    viewName: 'silta-view-name',
    viewType: 'silta-view-type'
}

const dynamicDesignator = '__silta-dynamic';

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

// A poor man's version of Clojure's `prewalk`
function walk(f, obj_) {
    const obj = f(obj_);
    if (Array.isArray(obj)) {
        return obj.map((x) => walk(f, x));
    } else if (typeof obj === "object") {
        return Object.fromEntries(Object.entries(obj).map(([key, value]) => [walk(f, key), walk(f, value)]));
    }
    return obj;
}

// TODO: handle cases like [:value ".some-selector"]
function parseConfig(config) {
    return config;
}

// rules for how to resolve things dynamically
const dynamicParamResolutions = {
    "value": (args) => document.querySelector(args[0]).value
}

// TODO: add documentation (input, output)
function parseEvent(event, viewId) {
    const thisViewSelector = `[silta-view-id="${viewId}"]`;
    const withoutConfig = Array.isArray(event[1]);
    const baseEndpoint = withoutConfig ? event[1] : event[2];
    let type, config, endpoint;

    // prepend, append, ...
    type = event[0];

    // event config (e.g., map with target of prepend)
    config = withoutConfig ?
        { target: viewId ? thisViewSelector : undefined } :
        {
            ... event[1],
            target: event[1].target ?
                event[1].target :
                (viewId ? thisViewSelector : undefined)
        };
    config = parseConfig(config); // resolve any dynamic inferences

    endpoint = baseEndpoint &&
        {
            method: 'GET',
            makeUrl: () => { // as fn to accomodate dynamic inferences
                const url = baseEndpoint[0];
                const params = walk((x) => {
                    if (Array.isArray(x) && x[0] === dynamicDesignator) {
                        const resolver = dynamicParamResolutions[x[1]];
                        const args = x.slice(2);
                        return resolver(args);
                    }
                    return x;
                }, baseEndpoint.slice(1));

                return makeRequestUrl(url, params);
            }
        };

    return { type, config, endpoint };
}

function queryEventfulNodes(node) {
    if (node instanceof HTMLElement) {
        const thisNode = node.attributes['silta-events'] && node;
        const childNodes = Array.from(node.querySelectorAll('[silta-events]'));
        
        return thisNode ? childNodes.concat(thisNode) : childNodes;
    }
}

function queryEventfulNodesInNodes(nodes) {
    const eventNodes = nodes.map(queryEventfulNodes)
                            .filter((x) => x) // drop undefineds
                            .flat(1);         // array of arrays -> flattened array of nodes

    // return unique nodes
    return [...new Set(eventNodes)];
}

function setupEventHandlers(nodes) {
    const eventNodes = queryEventfulNodesInNodes(nodes);

    if (eventNodes.length === 0) {
        console.log("No events for nodes, skipping:", nodes);
        return;
    }

    // iterate over unique nodes
    eventNodes.forEach((node) => {
        const stringifiedEvents = node.getAttribute(specialAttrs.events);

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
                node.addEventListener(eventType, (e) => {
                    e.preventDefault();
                    console.log(`Calling ${callbacks.length} '${eventType}' callbacks for node:`, node);
                    callbacks.forEach((cb) => cb());
                });
            });
        } catch (error) {
            console.error("Failed to parse events for node:", node);
            console.error(error);
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
    xhr.open(req.method, req.makeUrl());
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
    "dom.node/remove": (_, elt) => {
        elt.remove();
        return [];
    },
    "dom.node/reset": (_, elt) => {
        elt.reset();
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
