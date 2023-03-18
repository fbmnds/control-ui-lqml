import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Window 2.12
//import QtCharts 2.0
import QtWebSockets 1.0
import QtQml 2.12

Window {
    id: main
    visible: true
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
                color: "blue"
            }

            onPressed: {
                background.color = "yellow";
                text = "Werkstattlicht ...";
                socket.wslRequest('/r1');
            }

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

            function b64DecodeUnicode(str) { return Qt.atob(str); }
            
            onSvgTextChanged: svgText64 = b64DecodeUnicode(svgText);
            onSvgText64Changed: {
                //b64Decode(svgText);
                console.log(svgText64.substring(0,30));
                //svg.source = "data:image/svg+xml;utf8," + svgText64;
                svg.source = svgText64;
            }
            onSvgMsgChanged: svgMsg64 = b64DecodeUnicode(svgMsg);
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

                function updateWerkstattlicht(status) {
                    if (status.startsWith('{ "r1" : 1 }'))
                    {
                        button.background.color = "green";
                        button.text = "Werkstattlicht AN";
                    }
                    else if (status.startsWith('{ "r1" : 0 }'))
                    {
                        button.background.color = "grey";
                        button.text = "Werkstattlicht AUS";                        
                    }
                    else
                    {
                        button.background.color = "red";
                        button.text = "Werkstattlicht ...";
                    }
                }

                onClientConnected: {
                    webSocket.onTextMessageReceived.connect(function(src) {
                        //rctMsgBox.setMessage("connected");
                        //rctMsgBox.appendMessage(src.substring(0,30));
                        let wsurl = webSocket.url.toString();
                        if (wsurl.endsWith('/werkstatt/status'))
                        {
                            console.log('received /werkstatt/status');
                            let jsrc = JSON.parse(src);
                            column.state = jsrc.wslStatus;
                            rctTempHum.svgText64 = rctTempHum.b64DecodeUnicode(jsrc.wsthSvg64);
                            rctTempHum.svgMsg64 = rctTempHum.b64DecodeUnicode(jsrc.wsthList64);
                        }
                        else if (wsurl.indexOf('/werkstattlicht') >= 0)
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
                                rctTempHum.svgText64 = rctTempHum.b64DecodeUnicode(jsrc.svg);
                                rctTempHum.svgMsg64 = rctTempHum.b64DecodeUnicode(jsrc.text);
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

            onTextMessageReceived: {
                console.log(url + ' response: ' + message);
                if (socket.url.toString().indexOf('/werkstattlicht/' >= 0)) {
                    socket.active = false;
                    tmSocket.running = false;
                    let jsrc = JSON.parse(message);
                    column.state = jsrc.wslStatus;
                    rctTempHum.svgText64 = rctTempHum.b64DecodeUnicode(jsrc.wsthSvg64);
                    rctTempHum.svgMsg64 = rctTempHum.b64DecodeUnicode(jsrc.wsthList64);
                    ;
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

        states: [
            State {
                name: "ON"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht AN";
                    background.color: "green"
                }
            },
            State {
                name: "OFF"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht AUS";
                    background.color: "grey"

                }
            },
            State {
                name: "WAIT"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht ...";
                    background.color: "yellow"
                }
            },
            State {
                name: "ERROR"
                PropertyChanges {
                    target: button;
                    text: "Werkstattlicht FEHLER";
                    background.color: "red"
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
