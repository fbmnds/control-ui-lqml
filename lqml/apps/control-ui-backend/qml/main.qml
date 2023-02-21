import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtCharts 2.0
import QtWebSockets 1.0

Item {
    id: main
    objectName: "main"
    width: Screen.width
    height: Screen.height

    Column {
        id: column
        objectName: "column"
        spacing: 10
        width: parent.width
        height: parent.height

        Label {
            id: label
            objectName: "label"
            font.pixelSize: 22
            width: parent.width
            height: 40

            text: "Control UI Backend"
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
        }

        Button {
            id: button
            objectName: "btnWerkstattLicht"

            width: parent.width
            height: 50

            text: "Licht"

            function set (clr) {
                background.color = clr
            }

            background: Rectangle {
                radius: 12
                color: "lightblue"
            }

            Timer {
                id: tmWerkstattLicht
                interval: 30000
                repeat: true
                running: true
                triggeredOnStart: true

                onTriggered: Lisp.call(this, "app:werkstattlicht")
            }

            onPressed: Lisp.call(this, "app:button-pressed")
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
            id: rctTempHum
            objectName: "rctTempHum"
            width: parent.width
            height: 200
            color: "lavender"

            property string svgText: ""
            property string svgText64: ""
            property string svgMsg: ""
            property string svgMsg64: ""

            property var bcQueue: []
            property var clientIP: ["192.168.178.23", "192.168.178.31"]
            property int client: 0
            property string wsmsg: ""
            property string bcUrl: ""
            property string bcMsg: ""

            property string wslStatus: '{ "r1" : 0 }'

            function setSvgText (src) {
                Lisp.call(this, "app:put-svg", src);
            }

            function triggerTransmitt (client) {
                console.log("broadcast..." + client + " " + clientIP.length);
                socket.url = "ws://" + clientIP[client] + ":7700" + bcUrl;
                rctTempHum.client = client + 1;
                socket.active = true;
                rctTempHum.wsmsg = bcMsg;
            }

            function addBroadcastEvent (url, msg) {
                bcQueue.push([url, msg]);
            }
            
            function broadcast() {
                if (client < clientIP.length)
                {
                    triggerTransmitt(client);
                }
                else
                {
                    client = 0;
                    tmSocket.running = false;
                    tmBroadcast.running = true;
                }
            }

            Timer {
                id: tmBroadcast
                interval: 3000
                repeat: true
                running: true
                triggeredOnStart: true
                onTriggered: {
                    //console.log('triggered: length ' + rctTempHum.bcQueue.length);
                    if (rctTempHum.bcQueue.length > 0)
                    {
                        running = false;
                        let bc = rctTempHum.bcQueue.shift();
                        console.log('url ' + bc[0] + " msg " + bc[1]);
                        rctTempHum.bcUrl = bc[0];
                        rctTempHum.bcMsg = bc[1];
                        rctTempHum.broadcast();
                    }
                }
            }

            onSvgTextChanged: svg.source = svgText
            onSvgMsgChanged: rctMsgBox.setMessage(svgText)

            Image {
                id: svg
                objectName: "svg"
                anchors.centerIn: parent
                source: "svg/simple-example2.svg"
            }

            WebSocketServer {
                id: server
                host: "0.0.0.0"
                port: 7700
                listen: true

                onClientConnected: {
                    webSocket.onTextMessageReceived.connect(function(src) {
                        let wsurl = webSocket.url.toString();
                        rctMsgBox.setMessage("client :'" + wsurl);
                        rctMsgBox.appendMessage(src.substring(0,20));
                        if (wsurl.endsWith('/werkstattlicht/?'))
                        {
                            webSocket.sendTextMessage(rctTempHum.wslStatus);
                        }
                        if (wsurl.endsWith('/werkstattlicht/r1'))
                        {
                            Lisp.call(this, "app:button-pressed");
                        }
                        if (src.startsWith("<?xml"))
                        {
                            svg.source = "data:image/svg+xml;utf8," + src;
                        }
                        else
                        {
                            //Lisp.call(this, "app:put-svg", src);
                            rctTempHum.setSvgText(src);
                            rctMsgBox.setMessage(rctTempHum.svgMsg);
                        }
                        //tmSocket.running = true;
                    });
                }
                onErrorStringChanged: {
                    svg.source = "svg/simple-example2.svg";
                }
            }

            Timer {
                id: tmSocket
                interval: 3000
                repeat: false
                running: false
                triggeredOnStart: false
                onTriggered: {
                    console.log("end of transmitt to " + socket.url);
                    socket.active = false;
                    rctTempHum.broadcast();
                }
            }

            WebSocket {
                id: socket
                url: ""

                onTextMessageReceived: {
                    rctMsgBox.appendMessage(message)
                }
                onStatusChanged: {
                    console.log("send to url " + socket.url);
                    tmSocket.running = true; // timeout for connection
                    if (socket.status == WebSocket.Error) {
                        rctMsgBox.appendMessage("Error: " + socket.url + socket.errorString)
                    } else if (socket.status == WebSocket.Open) {
                        console.log("Socket open, sending...");
                        socket.sendTextMessage(rctTempHum.wsmsg);
                    } else if (socket.status == WebSocket.Closed) {
                        console.log("Socket closed for " + socket.url)
                    }
                }
                active: false
            }
        }

        Rectangle {
            id: rctMsgBox
            objectName: "txtMsgBox"
            width: parent.width
            height: 260
            color: "lavender"

            function appendMessage(message) {
                txtMsgBox.text += "\n" + message
            }

            function setMessage(message) {
                txtMsgBox.text = message
            }

            Text {
                id: txtMsgBox
                objectName: "txtMsgBox"
                font.pointSize: 12
                anchors.verticalCenter: parent.verticalCenter
                anchors.horizontalCenter: parent.horizontalCenter
                lineHeight: 1.1
                text: "Waiting...\n"
            }
        }
    }
}
