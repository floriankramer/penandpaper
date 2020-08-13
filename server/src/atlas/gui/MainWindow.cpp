#include "MainWindow.h"

#include <QAction>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>

#include "Conversions.h"
#include "core/Map.h"
#include "rendering/TileRenderer.h"

namespace atlas {
MainWindow::MainWindow() {
  buildMenu();
  buildUi();
}

void MainWindow::buildUi() {
  _map = std::make_shared<Map>();
  // initialize a 500x500m map
  _map->initialize(1000, 1000, 25);

  _map_widget = new MapWidget();
  _map_widget->setMap(_map);

  setCentralWidget(_map_widget);
}

void MainWindow::buildMenu() {
  QMenu *file = menuBar()->addMenu("file");

  QAction *newMap = new QAction("New Map");

  file->addAction(newMap);
}
}  // namespace atlas
