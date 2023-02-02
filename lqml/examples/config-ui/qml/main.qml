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

            function set (clr) {
                background.color = clr
            }
            
            background: Rectangle {
          	radius: 2
          	color: "lightblue"
            }

	    Timer {
       		id: textTimer1
       		interval: 150000
       		repeat: true
       		running: true
       		triggeredOnStart: true
       		onTriggered: Lisp.call(this, "app:werkstattlicht")
   	    }
	    
            onPressed: Lisp.call(this, "app:button-pressed")
      	}
        


	Rectangle {
	    id: rect2
   	    width: parent.width
   	    height: 30
   	    color: "lavender"
	    
   	    Text {
       		id: foo2
       		font.pointSize: 12
       		anchors.verticalCenter: parent.verticalCenter
       		anchors.horizontalCenter: parent.horizontalCenter
       		
       		function set() {
           	    foo2.text = Date()
       		}
   	    }
	    
   	    Timer {
       		id: textTimer2
       		interval: 1000
       		repeat: true
       		running: true
       		triggeredOnStart: true
       		onTriggered: foo2.set()
   	    }	
	}
        
        Rectangle {
	    id: rect3
   	    width: parent.width
   	    height: 200
   	    color: "lavender"


            Image {
                id: svg
                anchors.centerIn: parent
                source: "svg/simple-example2.svg"
            }
	}
    }
}
