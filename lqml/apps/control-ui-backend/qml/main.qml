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
                interval: 150000
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
            property string wsmsg: ""

            property var clients: ["192.168.178.23", "192.168.178.31"]

            function setSvgText (src) {
                Lisp.call(this, "app:put-svg", src);
            }
            
            onSvgTextChanged: svg.source = svgText
            onSvgMsgChanged: rctMsgBox.setMessage(svgText)

            onSvgText64Changed: {
                //console.log(svgText2);
                socket.active = true;
                rctTempHum.wsmsg = '{ "tag": "data", "text": "' + svgMsg
                    + '", "svg": "' + svgText2 + '" }';
            }

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
                        rctMsgBox.setMessage("client :'" + webSocket.url.toString());
                        rctMsgBox.appendMessage(src.substring(0,20));
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
                onTriggered: socket.active = false
            }

            WebSocket {
                id: socket
                url: "ws://192.168.178.31:7700"

                onTextMessageReceived: {
                    rctMsgBox.appendMessage(message)
                }
                onStatusChanged: if (socket.status == WebSocket.Error) {
                    rctMsgBox.appendMessage("Error: " + socket.errorString)
                } else if (socket.status == WebSocket.Open) {
                    socket.sendTextMessage(rctTempHum.wsmsg);
                    console.log("Socket open, sending...")
                    tmSocket.running = true;
                } else if (socket.status == WebSocket.Closed) {
                    console.log("Socket closed")
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
