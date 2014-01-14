package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.dialogs.OutputDialog;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JLabel;



/**
 * The Controller executes the corresponding methods on molecules View clicking events.
 * It updates the molecules model and the molecules view, and sends commands to Biocham.
 * 
 * */
public class MoleculesController implements ActionListener{

	MoleculesModel moleculesModel;
	MoleculesView moleculesView;	
	ArrayList<String> checkMolecules;
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public MoleculesController(MoleculesModel model, MoleculesView view){
		moleculesModel=model;
		moleculesView=view;			
		checkMolecules=new ArrayList<String>();//((ParamTableMolecules)moleculesModel.getBiochamModel().getMolecules().getParamTable()).getCheckMolecules();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("checkMolecules")){
			checkMolecules();
		}
		
	}

	public void checkMolecules() {
		moleculesModel.getBiochamModel().sendToBiocham("check_molecules.\n");
		((ParamTableMolecules)moleculesModel.getBiochamModel().getMolecules().getParamTable()).checkMolecules(moleculesModel.getBiochamModel().molsF);
	}

	public MoleculesModel getMoleculesModel() {
		return moleculesModel;
	}

	public void setMoleculesModel(MoleculesModel moleculesModel) {
		this.moleculesModel = moleculesModel;
	}

	public MoleculesView getMoleculesView() {
		return moleculesView;
	}

	public void setMoleculesView(MoleculesView moleculesView) {
		this.moleculesView = moleculesView;
	}

	
	
}
