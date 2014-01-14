package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;



public class OutputDialog extends JDialog{

	
	Container contents;
	GradientPanel coverPanel;
	
	
	public OutputDialog(String title, JFrame parent){
		
		 super (parent, true);			
		 setTitle(title);		 
		 contents = getContentPane();
		 contents.setLayout(new BorderLayout());
		 coverPanel=new GradientPanel();
		// coverPanel.setBackground(Utils.backgroundColor);
		 BoxLayout bl=new BoxLayout(coverPanel, BoxLayout.Y_AXIS);	
		 coverPanel.setLayout(bl);
		
		// btnsPanel=new GradientPanel();
		// btnsPanel.setBackground(Utils.backgroundColor);
		// contents.add(btnsPanel,BorderLayout.SOUTH);
		
		 contents.add(coverPanel,BorderLayout.CENTER);
		// btnsPanel.add(okButton);  
		 
   	  		  	 	
   	  	
	}
	
	public void showVisible(){
	     
  	  	 JFrame frame=BiochamMainFrame.frame;
  	  	 Point pos = frame.getLocationOnScreen();
  	  	 setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
  	  	 setResizable(true);   	  	 	    
  	  	 setLocationRelativeTo(frame);
  	  	 setVisible(true);	 
	     pack();
	}
	public GradientPanel getCoverPanel() {
		return coverPanel;
	}

	public void setCoverPanel(GradientPanel coverPanel) {
		this.coverPanel = coverPanel;
	}
}
