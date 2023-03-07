import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtCharts 2.0


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
            objectName: "button"

            width: parent.width
            height: 50
            text: "Licht"
            background: Rectangle {
                radius: 12
                color: "lightblue"
            }

            onPressed: Lisp.call("app:button-pressed")

            //Component.onCompleted: {  }
        }

//        Rectangle {//{{{
//            id: rctTime
//            width: parent.width
//            height: 30
//            color: "lavender"
//
//            Text {
//                id: txtTime
//                font.pointSize: 12
//                anchors.verticalCenter: parent.verticalCenter
//                anchors.horizontalCenter: parent.horizontalCenter
//
//            }
//
//            Timer {
//                id: tmTime
//                interval: 1000
//                repeat: true
//                running: true
//                triggeredOnStart: true
//                onTriggered: txtTime.text = Date()
//            }
//        }//}}}
    }
}
