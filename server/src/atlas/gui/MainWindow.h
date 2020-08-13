#pragma once

#include <QMainWindow>
#include <memory>

#include "MapWidget.h"
#include "core/Map.h"


namespace atlas {
class MainWindow: public QMainWindow {
  Q_OBJECT
public:
  MainWindow();

private:
  void buildUi();
  void buildMenu();

  MapWidget *_map_widget;

  std::shared_ptr<Map> _map;
};
}
