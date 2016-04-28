/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.demo.swing;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class GreetingApplication extends JFrame {

    private static final long serialVersionUID = 1L;

    public GreetingApplication() {
        JPanel panel = new JPanel(new GridLayout(2, 1));
        getContentPane().add(panel);

        // Create input area
        JPanel inputArea = new JPanel(new GridLayout(1, 3));
        panel.add(inputArea);
        inputArea.add(new JLabel("Your greeting:"));
        final JTextField input = new JTextField("");
        input.setName("text.input");
        inputArea.add(input);
        JButton button = new JButton("Greet");
        button.setName("button.greet");
        inputArea.add(button);

        // Create output area
        JPanel outputArea = new JPanel();
        panel.add(outputArea);
        final JTextField greeting = new JTextField("Greet me!");
        greeting.setName("text.output");
        greeting.setEnabled(false);
        outputArea.add(greeting);

        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setSize(500, 200);
        setVisible(true);

        button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                greeting.setText(input.getText());
            }
        });
    }
    
    public static void main(String[] args) {
        new GreetingApplication();
    }
    
    public String sayHello() {
    	return "Hello Service";
    }

}
