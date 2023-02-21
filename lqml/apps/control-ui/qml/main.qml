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

            text: "Control UI"
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
        }

        Button {
            id: button
            objectName: "btnWerkstattLicht"

            width: parent.width
            height: 50
            text: "Licht"
            background: Rectangle {
                radius: 12
                color: "lightblue"
            }

            function set (clr) {
                background.color = clr
            }

            onPressed: { Lisp.call(this, "app:button-pressed") }

            Component.onCompleted: { socket.wslRequest('/?') }
        }

        Rectangle {//{{{
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
        }//}}}

        Rectangle {//{{{
            id: rctTempHum
            objectName: "rctTempHum"
            width: parent.width
            height: 200
            color: "lavender"

            property string svgText: ""
            property string svgText64: ""
            property string svgMsg: ""
            property string svgMsg64: ""

            function b64Decode (symb, txt) {
                Lisp.call(this,"app:b64-decode", symb, txt);
            }

            onSvgTextChanged: b64Decode("svgText64", svgText);
            onSvgText64Changed: {
                        //b64Decode(svgText);
                        console.log(svgText64.substring(0,30));
                        svg.source = "data:image/svg+xml;utf8," + svgText64;
}
            onSvgMsgChanged: b64Decode("svgMsg64", svgMsg);
            onSvgMsg64Changed: rctMsgBox.setMessage(svgMsg64)

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

                function updateWerkstattlicht(src) {
                    Lisp.call(this, "app:werkstattlicht", src);
                }

                onClientConnected: {
                    webSocket.onTextMessageReceived.connect(function(src) {
                        //rctMsgBox.setMessage("connected");
                        rctMsgBox.appendMessage(src.substring(0,30));
                        let wsurl = webSocket.url.toString();
                        if (wsurl.indexOf('/werkstattlicht') >= 0)
                        {
                            updateWerkstattlicht(src);
                        }
                        else if (src.startsWith("<?xml"))
                        {
                            svg.source = "data:image/svg+xml;utf8," + src;
                        }
                        else if (src.startsWith("data:image/svg"))
                        {
                            svg.source = src;
                        }
                        else
                        {
                            console.log(src);
                            let jsrc = JSON.parse(src);
                            if (jsrc.tag == "data") {
                                rctTempHum.b64Decode("svgText64", jsrc.svg);
                                rctTempHum.b64Decode("svgMsg64", jsrc.text);
                            }
                        }
                    });
                }
                onErrorStringChanged: {
                    svg.source = "svg/simple-example2.svg";
                }
            }
        }//}}}


        Timer {
            id: tmSocket
            interval: 6000
            repeat: false
            running: false
            triggeredOnStart: false

            onTriggered: {
                console.log("end of transmitt to " + socket.url);
                socket.active = false;
            }
        }

        WebSocket {
            id: socket
            objectName: "wsclient"
            url: ""
            active: false

            property string msg: ""

            function wslRequest (slug) {
                socket.url = "ws://192.168.178.6:7700/werkstattlicht" + slug;
                socket.active = true;
                socket.msg = ':ignore';
            }

            function setStatus (msg) { Lisp.call(this, "app:set-status", msg) }

            onTextMessageReceived: {
                console.log(url + ' response: ' + message);
                if (socket.url.toString().indexOf('/werkstattlicht/' >= 0)) {
                    socket.active = false;
                    tmSocket.running = false;
                    setStatus(message);
                }
            }
            onStatusChanged: {
                console.log("send to url " + socket.url);
                tmSocket.running = true; // timeout for connection
                if (socket.status == WebSocket.Error)
                {
                    rctMsgBox.appendMessage("Error: "
                                            + socket.url + socket.errorString)
                    socket.active = false;
                    tmSocket.running = false;
                }
                else if (socket.status == WebSocket.Open)
                {
                    console.log("Socket open, sending: ");
                    socket.sendTextMessage(socket.msg);
                }
                else if (socket.status == WebSocket.Closed)
                {
                    console.log("Socket closed for " + socket.url)
                    socket.active = false;
                    tmSocket.running = false;
                }
            }
        }


        Rectangle {//{{{
            id: rctMsgBox
            objectName: "tctMsgBox"
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
                text: "Waiting..."
            }
        }//}}}
    }
}
