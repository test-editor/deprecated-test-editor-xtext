package org.testeditor.demo.swing

# GreetingUITest implements GreetingSpec
 
* Start the famous greetings application. 
 
    Mask: GreetingApplication
    - Starte Anwendung "org.testeditor.demo.swing.GreetingApplication"
 
* Send greetings "Hello World" to the world.
 
    Mask: GreetingApplication
    - Gebe "Hello World" in das Feld <Input> ein 
    - Klicke auf <GreetButton>
    - Warte "2000" ms   
    - foo = Lese den Text von <Output>
    - assert foo == "Hello World"
 
* Stop the famous greeting application.

	Mask: GreetingApplication
	- Stoppe Anwendung