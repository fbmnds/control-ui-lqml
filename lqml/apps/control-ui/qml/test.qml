// Copyright (C) 2016 Kurt Pattyn <pattyn.kurt@gmail.com>.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause
import QtQuick 2.0
import QtWebSockets 1.0

Rectangle {
    id: rect
    width: 360
    height: 360
    
    property string svg: "<svg version=\\\"1.1\\\" width=\\\"300\\\" height=\\\"200\\\" xmlns=\\\"http://www.w3.org/2000/svg\\\"> <rect width=\\\"100%\\\" height=\\\"100%\\\" fill=\\\"lavender\\\" /> <circle cx=\\\"150\\\" cy=\\\"100\\\" r=\\\"80\\\" fill=\\\"lightgrey\\\" /> <text x=\\\"150\\\" y=\\\"125\\\" font-size=\\\"60\\\" text-anchor=\\\"middle\\\" fill=\\\"red\\\">SVG</text> </svg>"
    property string msg:  "..."

    
    WebSocket {
        id: socket
        url: "ws://192.168.178.8:7700"

        onTextMessageReceived: {
            messageBox.text = messageBox.text + "\nReceived message: " + message
        }
        onStatusChanged: if (socket.status == WebSocket.Error) {
            console.log("Error: " + socket.errorString)
        } else if (socket.status == WebSocket.Open) {
            socket.sendTextMessage(rect.msg)
        } else if (socket.status == WebSocket.Closed) {
            messageBox.text += "\nSocket closed"
        }
        active: false
    }

    Text {
        id: messageBox
        text: socket.status == WebSocket.Open ? qsTr("Sending...") : qsTr("Welcome!")
        anchors.centerIn: parent
    }

    MouseArea {
        property bool status: true
        anchors.fill: parent
        onClicked: {
            socket.active = !socket.active;

            if (!socket.active)
            {
                if(status)
                {
                    rect.msg = '{ "tag": "svg", "svg": "' + rect.svg + '" }';
                } else {
                    rect.msg = '{ "tag": "text" }'
                }
                status = !status;
            }
            //            socket.active = !socket.active;
            //            //secureWebSocket.active =  !secureWebSocket.active;
            //Qt.quit();
        }
    }
}
