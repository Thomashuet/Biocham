package fr.inria.contraintes.biocham.customComponents;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;


public class CustomOptionPane extends JOptionPane{
	JDialog d;
	
	public CustomOptionPane(JFrame f,
            String s1,
            String title,
            int type,
            Icon o1,
            Object[] o2,
            String msg
            ){
		super();
		d=this.createDialog(f, title);
		d.setLocationRelativeTo(f);
		this.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.setMessage(s1);
		this.setIcon(o1);
		this.setSelectionValues(o2);
		this.setInitialSelectionValue(msg);
		
		/*
		 * public static Object showInputDialog(Component parentComponent,
                                     Object message,
                                     String title,
                                     int messageType,
                                     Icon icon,
                                     Object[] selectionValues,
                                     Object initialSelectionValue)
		 **/
	}
}
