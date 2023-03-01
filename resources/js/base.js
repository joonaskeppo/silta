// ---- base ---

const eventsAttr = 'silta-events';
const eventsSelector = '[' + eventsAttr + ']';

const clientId = self.crypto.randomUUID();

function findElementsWithEventsUnderNode(node) {
    let entries = [];

    if (node.hasAttribute(eventsAttr)) {
        entries.push(node);
    }

    node.querySelectorAll(eventsSelector).forEach((elt) => {
        entries.push(elt);
    });

    return entries;
}

// event handlers are provided as `silta-events`
// as map of 2d arrays
function findElementsWithEvents(rootNodeOrSelector) {
    if (rootNodeOrSelector instanceof HTMLElement) {
        return findElementsWithEventsUnderNode(rootNodeOrSelector);
    }

    let entries = [];
    document.querySelectorAll(rootNodeOrSelector).forEach((rootNodeOrSelector) => {
        entries = entries.concat(findElementsWithEventsUnderNode(rootNodeOrSelector));
    });
    return entries;
}

function makeQueryString(params) {
    return '?' + Object.keys(params).map(k => k + '=' + params[k]).join('&');
}

function makeRequestUrl(endpoint, params) {
    return endpoint + makeQueryString({"__params": JSON.stringify(params)});
}

function setupEventHandlers(rootNodeOrSelector) {
    findElementsWithEvents(rootNodeOrSelector).forEach((node) => {
        try {
            const events = JSON.parse(node.getAttribute(eventsAttr));
            Object.keys(events).forEach((eventType) => {
                let callbacks = [];

                events[eventType].forEach((descriptor) => {
                    const type = descriptor[0];
                    const eventConfig = descriptor[1] instanceof Array ? undefined : descriptor[1];
                    const responseHandler = {
                        fn: handleUpdate(elementUpdates[type]),
                        // TODO: ensure that node always has id attribute! (when dynamic)
                        args: [(eventConfig && eventConfig.target) ? eventConfig.target : '#' + node.id]
                    };
                    const endpointWithParams = descriptor[descriptor.length - 1];
                    const requestConfig = {
                        method: 'GET',
                        url: makeRequestUrl(endpointWithParams[0], endpointWithParams.slice(1))
                    };

                    console.log(`Adding '${eventType}' type listener for node:`, node);
                    console.log(`Initializing '${type}' action:`, eventConfig, requestConfig, responseHandler);

                    callbacks.push(() => handleAjaxRequest(requestConfig, responseHandler));
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
    });
}

function handleAjaxRequest(req, props) {
    let xhr = new XMLHttpRequest();


    // response handling
    xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            props.fn.apply(null, props.args)(xhr.responseText);
        }
    }
    // send request
    xhr.open(req.method, req.url);
    xhr.setRequestHeader('client-id', clientId);
    xhr.send();
}

// --- handlers ---

// TODO: ensure that selector is always available (and unique when target is 'this')

const elementUpdates = {
    swap: { handler: (target, newNode) => target.replaceWith(newNode) },
    append: { handler: (target, newNode) => target.append(newNode) },
    prepend: { handler: (target, newNode) => target.prepend(newNode) },
    remove: { noElementCreation: true, skipEventHandlerRefresh: true, handler:(target) => target.remove() } 
}

function handleUpdate(updateConfig) {
    return function(selector) {
        return function(html) {
            let nodes = [];

            document.querySelectorAll(selector).forEach((elt) => {
                let newElt;
                if (!updateConfig.noElementCreation) {
                    // create new temp node, will be pushed to `nodes`
                    newElt = document.createElement('div');
                    newElt.innerHTML = html;
                    newElt = newElt.firstChild;

                    // do update
                    updateConfig.handler(elt, newElt);
                } else {
                    // no new element to create, so just call handler
                    updateConfig.handler(elt);
                }

                // for `delete`, we don't want to refresh our event handlers
                if (!updateConfig.skipEventHandlerRefresh) {
                    nodes.push(newElt || elt);
                }

                // refresh event handlers
                console.log("Refreshing event handlers for:", nodes);
                nodes.forEach((node) => setupEventHandlers(node));
            })
        }
    }
}

// Initialize with root-most node, <body>
setupEventHandlers('body');
