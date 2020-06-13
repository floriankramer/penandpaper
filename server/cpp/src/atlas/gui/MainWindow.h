#pragma once


#include <QMainWindow>

namespace atlas {
class MainWindow: public QMainWindow {
  Q_OBJECT
public:
  MainWindow();

private:
  void buildUi();
  void buildMenu();
};
}
