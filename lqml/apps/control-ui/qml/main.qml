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

            onSvgTextChanged: {
                Lisp.call(this,"app:b64-decode", svgText2);
                console.log(svgText2.substring(0,30));
                svg.source = "data:image/svg+xml;utf8," + svgText2
            }
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
                        rctMsgBox.setMessage("connected");
                        rctMsgBox.appendMessage(src);
                        if (src.startsWith("<?xml"))
                        {
                            svg.source = "data:image/svg+xml;utf8," + src;
                        }
                        else if (src.startsWith("data:image/svg"))
                        {
                            console.log(src.substring(0,30));
                            svg.source = src;
                        }
                        else
                        {
                            console.log(src);
                            let jsrc = JSON.parse(src);
                            if (jsrc.tag == "svg") {
                                svg.source = jsrc.data;
                                rctMsgBox.setMessage(jsrc.text);
                            }
                        }
                    });
                }
                onErrorStringChanged: {
                    svg.source = "svg/simple-example2.svg";
                }
            }
        }

        Rectangle {
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
        }
    }
}
