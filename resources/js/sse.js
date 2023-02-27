// --- sse ---

var source = new EventSource("/stream");

source.onmessage= (e) => {
    const data = e.data; // TODO: for now...
    // TODO: temp flow for demo purposes
    if (data !== "ping") {
        let temp = document.createElement('div');
        temp.innerHTML = data;
        temp = temp.firstChild;

        const sinkId = temp.getAttribute('silta-sink-id');
        console.log("Replacing sink", sinkId, data);

        // handle update
        document.querySelector(`[silta-sink-id="${sinkId}"]`).replaceWith(temp);
    }

    if (data.msg == "end") {
        console.log("Closed!");
        source.close();
    }
};

source.onopen = (e) => {
    console.log("Connection opened:", e);
};

source.onerror = (e) => {
    console.log("Connection error:", e);
    if (e.readyState == EventSource.CLOSED) {
        console.log("Connection closed:", e);
    }
    source.close();
};
