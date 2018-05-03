function inputIntro() {
  var inputGuide = introJs();

  $("body").removeClass("sidebar-collapse");

  inputGuide.setOptions({
    steps: [
      {
        element: '.sidebar-toggle',
        intro: 'Open and close the sidebar menu.'
      },
      {
        element: '.sidebar-menu',
        intro: "Navigate between sections using tabs.",
        positions: 'bottom-middle-aligned'
      },
      {
        element: '#excelBox',
        intro: "Upload your filled excel input template. You should have received this template along with the software.",
        position: 'bottom-middle-aligned'
      },
      {
        element: "#numSimBox",
        intro: "Set the number of simulations. The recommended number of simulations is 1'000'000. An excessive number of simulation could trigger an error message if not enough memory is available.",
        position: "bottom-middle-aligned"
      },
      {
        element: '#runSim',
        intro: "Click on this button to run the simulation.",
        position: 'bottom-middle-aligned'
      }
    ],
    showStepNumbers: false,
    showProgress: true
  });

  inputGuide.start();
}

function resultsIntro() {
  var resultsGuide = introJs();

  $("body").removeClass("sidebar-collapse");


    resultsGuide.setOptions({
    steps: [
      {
        element: '#warnLog',
        intro: 'Download the warning log. The warning log will ensure that the excel read-in and computations runned as intended.',
        position: 'top'
      },
      {
        element: '#noScenario',
        intro: 'Results without scenario aggregation.',
        position: 'bottom-middle-aligned'
      },
      {
        element: '.nav-tabs',
        intro: 'You can use these tabs to navigate between different risk simulations. &Delta;RBC stands for change in risk-bearing capital and is the result of the aggregated risks.',
        position: 'bottom-middle-aligned'
      },
      {
        element: '#plotBox',
        intro: 'Empirical density of the change in risk-bearing capital produced by the monte-carlo simulations for the selected risk. Positive in a profit and negative is a loss.',
        position: 'bottom-middle-aligned'
      },
      {
        element: '#highlightBoxes',
        intro: 'Display of the solvency figures.',
        position: 'top'
      },
      {
        element: '#scenario',
        intro: 'Results with scenario aggregation.',
        position: 'bottom-middle-aligned'
      },
      {
        element: '.tableBox',
        intro: "Keep the cursor over one of the outputs to get additional comments.",
        position: 'top'
      },
      {
        element: '#keep',
        intro: 'Check which additional simulation vectors to save.',
        position: 'bottom-middle-aligned'
      },
      {
        element: document.getElementById("results.xlsx"),
        intro: 'Download the results as an Excel file. It contains the displayed tables and the selected vectors of simulations.',
        position: 'top'
      }
    ].filter(function(obj) { return $(obj.element).length; }),
    showStepNumbers: false,
    showProgress: true
  });

  resultsGuide.start();
}
