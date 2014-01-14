package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JTextField;




/**
 * Class that creates a custom JTextField component which when double-clicked opens a dialog that assist the user in composing the value.
 * 
 * @author Dragana Jovanovska  
 */ 
public class TextFieldRules extends JTextField implements MouseListener{

	BiochamModel model;
	
	public TextFieldRules(String text,BiochamModel m){
		super(text);
		addMouseListener(this);
		model=m;
	}
	
	public TextFieldRules(BiochamModel m){
		super();
		addMouseListener(this);
		model=m;
	}
	
	public void mouseClicked(MouseEvent e) {
		if(e.getClickCount()==2){
			DialogAddSpecification params=new DialogAddSpecification(BiochamMainFrame.frame,model.getRules(), "Rules Operators",model.getRules().getParametersPanel());					
			String value=params.getFormula();
			setText(value);	
		}
		
	}

	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

}
