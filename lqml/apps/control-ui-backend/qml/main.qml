import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtCharts 2.0
import QtWebSockets 1.0

Column {
    id: main
    objectName: "main"
    width: Screen.width
    height: Screen.height

    Label {
        id: label
        objectName: "header"
        font.pixelSize: 22
        width: parent.width
        height: 60

        text: "Control UI Backend"
        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter
    }

    WebSocketServer {
        id: server
        objectName: "server"
        host: "0.0.0.0"
        port: 7700
        listen: true

        onClientConnected: {
            webSocket.onTextMessageReceived.connect(function(src) {
                Lisp.call("app:websocket-server-connect", src, webSocket)
            });
        }
        onErrorStringChanged: {
            let wsurl = webSocket.url.toString();
            if (wsurl.indexOf('/werkstattlicht/') < 0)
            {
                svg.source = "svg/simple-example2.svg";
            }
        }
    }

    WebSocket {
        id: socket
        objectName: "socket"
        url: ""
        active: false

        Timer {
            id: tmSocket
            objectName: "tmSocket"
            interval: 3000
            repeat: false
            running: false
            triggeredOnStart: false
            onTriggered: {
                console.log("connection timeout for " + socket.url);
                socket.active = false;
                wsth_svg.broadcast();
            }
        }

        property bool connecting: false
        property bool open: false
        property bool closing: false
        property bool closed: false
        property bool error: false

        property string msg: '{}'

        onMsgChanged: wsth_svg.broadcast()
        onTextMessageReceived: {
            wsth_list.appendMessage(message)
        }
        onStatusChanged: {
            socket.connecting = (socket.status == WebSocket.Connecting);
            socket.open = (socket.status == WebSocket.Open);
            socket.closing = (socket.status == WebSocket.Closing);
            socket.closed = (socket.status == WebSocket.Closed);
            socket.error = (socket.status == WebSocket.Error);
            Lisp.call("websocket-client-connect");
        }
    }

    Column {
        id: frontpage
        objectName: "frontpage"
        spacing: 10
        width: parent.width
        height: parent.height

        Button {
            id: button
            objectName: "wsl_button"
            width: parent.width
            height: 50
            text: "Licht"
            background: Rectangle {
                radius: 12
                color: "lightblue"
            }

            Timer {
                id: wsl_timer
                interval: 30000
                repeat: true
                running: true
                triggeredOnStart: true

                onTriggered: Lisp.call("app:werkstattlicht", '/?')
            }

            onPressed: Lisp.call("app:werkstattlicht", '/r1')
        }

        Rectangle {
            id: rctTime
            width: parent.width
            height: 30
            color: "lavender"

            Text {
                id: txtTime
                font.pointSize: 12
                anchors.verticalCenter: parent.verticalCenter
                anchors.horizontalCenter: parent.horizontalCenter
            }

            Timer {
                id: tmTime
                interval: 1000
                repeat: true
                running: true
                triggeredOnStart: true
                onTriggered: txtTime.text = Date()
            }
        }

        Rectangle {
            id: wsth_svg
            objectName: "wsth_svg"
            width: parent.width
            height: 200
            color: "lavender"

            property var clientIP: ["192.168.178.23", "192.168.178.31"]
            property int client: 0
            
            function setSvgText (src) {
                Lisp.call(this, "app:put-svg", src);
            }

            function broadcast() {
                if (client < (2 * clientIP.length - 1))
                {
                    // on connecting: tmSocket.running = true;
                    socket.url = "ws://"
                        + clientIP[client % clientIP.length] + ":7700/werkstatt/status";
                    socket.active = true;
                    client += 1;
                }
                else
                {
                    client = 0;
                    socket.active = false;
                }
            }

            function broadcastCycle() {
                if (bcCycle < 1)
                {
                    bcCycle = 1;
                }
                else
                {
                    bcCycle = 2;
                }
            }

            onSvgTextChanged: svg.source = svgText
            onSvgMsgChanged: wsth_list.setMessage(svgText)

            Image {
                id: svg
                objectName: "svg"
                anchors.centerIn: parent
                source: "svg/simple-example2.svg"
            }
        }

        Rectangle {
            id: wsth_list
            objectName: "wsth_list"
            width: parent.width
            height: 260
            color: "lavender"

            function appendMessage(message) {
                wsth_list_text.text += "\n" + message
            }

            function setMessage(message) {
                wsth_list_text.text = message
            }

            Text {
                id: wsth_list_text
                objectName: "wsth_list_text"
                font.pointSize: 12
                anchors.verticalCenter: parent.verticalCenter
                anchors.horizontalCenter: parent.horizontalCenter
                lineHeight: 1.1
                text: "Waiting...\n"
            }
        }

        property string mWsthSvg: ''
        property string mWsthList: ''

        property string mWsthSvg64: ''
        property string mWsthList64: ''
        property string mWslStatus: "WAIT" // ON OFF ERROR

        function jsonModelUpdate () {
            socket.msg =
                '{ "wsthSvg64": "' + mWsthSvg64 +
                '", "wsthList64": "' + mWsthList64 +
                '", "wslStatus": "' + mWslStatus +
                '" }';
        }
        onMWsthSvgChanged : Lisp.call("app:b64-encode", wsth_svg,
                                      "mWsthSvg64", wsth_svg.mWsthSvg)
        onMWsthListChanged : Lisp.call("app:b64-encode", wsth_svg,
                                       "mWsthList64", wsth_svg.mWsthList)

        onMWsthSvg64Changed : jsonModelUpdate()
        onMWsthList64Changed : jsonModelUpdate()
        onMWslStatusChanged : jsonModelUpdate()

        states: [
            State {
                name: "ON"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht AN";
                    background.color: "lightgreen"
                }
            },
            State {
                name: "OFF"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht AUS";
                    background.color: "lightgrey"

                }
            },
            State {
                name: "WAIT"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht ...";
                    background.color: "lightyellow"
                }
            },
            State {
                name: "ERROR"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht FEHLER";
                    background.color: "lightpink"
                }
            }
        ]

        transitions: [
            Transition {
                from: "*"; to: "*"
                PropertyAction { target: button; properties: "text,background.color" }
            }
        ]
    }
}
