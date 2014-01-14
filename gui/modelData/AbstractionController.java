package fr.inria.contraintes.biocham.modelData;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author Dragana Jovanovska
 *
 */

public class AbstractionController implements ActionListener{

	
	AbstractionModel abstractionModel;
	AbstractionView abstractionView;	
	

	
	public AbstractionController(AbstractionModel model, AbstractionView view){
		abstractionModel=model;
		abstractionView=view;			
			
	}

	
	public void actionPerformed(ActionEvent e) {
		
		
	}


	
	public AbstractionModel getAbstractionModel() {
		return abstractionModel;
	}
	public AbstractionView getAbstractionView() {
		return abstractionView;
	}

}
