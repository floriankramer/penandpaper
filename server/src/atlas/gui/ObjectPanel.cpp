#include "ObjectPanel.h"

#include <QDialog>
#include <QHBoxLayout>
#include <QIntValidator>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>

#include "MapWidget.h"

namespace atlas {

ObjectPanel::ObjectPanel(QWidget *parent) : QWidget(parent), _map(nullptr) {
  _object_index.load("objects");
  createUi();
}

ObjectPanel::~ObjectPanel() {}

void ObjectPanel::setMap(std::shared_ptr<Map> map) {
  if (_map.get() != map.get()) {
    _map = map;
    _map->listenTo(
        Map::EVENT_NEW_DISTRIBUTION,
        [this](const std::string &event) { this->updateDistributions(); });
    _map->listenTo(
        Map::EVENT_REMOVED_DISTRIBUTION,
        [this](const std::string &event) { this->updateDistributions(); });
  }
  updateDistributions();
}

void ObjectPanel::setMapWidget(MapWidget *widget) { _map_widget = widget; }

void ObjectPanel::createUi() {
  QVBoxLayout *layout = new QVBoxLayout();

  QHBoxLayout *button_layout = new QHBoxLayout();

  QPushButton *new_distribution = new QPushButton("+");
  connect(new_distribution, &QPushButton::pressed, [this] {
    if (_map == nullptr) {
      return;
    }

    this->createNewDistribution();
  });
  button_layout->addWidget(new_distribution);

  QPushButton *delete_distribution = new QPushButton("-");
  layout->addLayout(button_layout);

  _distributions_list = new QListWidget();

  layout->addWidget(_distributions_list, 1);

  setLayout(layout);

  connect(delete_distribution, &QPushButton::pressed, [this] {
    if (_map != nullptr) {
      int selected = _distributions_list->currentRow();
      _map->removeObjectDistribution(selected);
      _map_widget->invalidateObjectLayer();
    }
  });
  button_layout->addWidget(delete_distribution);
}

void ObjectPanel::updateDistributions() {
  if (_map == nullptr) {
    return;
  }
  _distributions_list->clear();
  for (const ObjectDistribution &dist : _map->objectDistributions()) {
    _distributions_list->addItem(QString::fromStdString(dist.name()));
  }
}

void ObjectPanel::createNewDistribution() {
  int64_t offset = 32;

  QDialog dialog;
  QVBoxLayout *layout = new QVBoxLayout();

  QLineEdit *name_edit = new QLineEdit();
  name_edit->setPlaceholderText("Name");
  layout->addWidget(name_edit);

  QListWidget *object_list = new QListWidget();
  for (const ObjectIndex::Object &obj : _object_index.objects()) {
    object_list->addItem(QString::fromStdString(obj.name));
  }
  object_list->setCurrentRow(0);

  layout->addWidget(object_list, 1);

  QGridLayout *settings_layout = new QGridLayout();
  settings_layout->addWidget(new QLabel("offset"), 0, 0);

  QLineEdit *offset_edit = new QLineEdit();
  offset_edit->setValidator(new QIntValidator(0, 9999));
  connect(offset_edit, &QLineEdit::editingFinished,
          [&offset, &offset_edit]() { offset = offset_edit->text().toInt(); });
  settings_layout->addWidget(offset_edit, 0, 1);

  layout->addLayout(settings_layout);

  QHBoxLayout *button_layout = new QHBoxLayout();
  QPushButton *cancel = new QPushButton("cancel");
  connect(cancel, &QPushButton::pressed, &dialog, &QDialog::reject);
  button_layout->addWidget(cancel);

  button_layout->addWidget(new QWidget(), 1);

  QPushButton *accept = new QPushButton("ok");
  connect(accept, &QPushButton::pressed, &dialog, &QDialog::accept);
  button_layout->addWidget(accept);

  layout->addLayout(button_layout);

  dialog.setLayout(layout);
  if (dialog.exec() != QDialog::Accepted) {
    return;
  }

  size_t object_index = object_list->currentRow();

  ObjectDistribution dist(_map->widthPixels(), _map->heightPixels(), offset,
                          _object_index.objects()[object_index].image);
  dist.setName(name_edit->text().toUtf8().constData());

  _map->addObjectDistribution(dist);
  // TODO: this is actually not required and only used for testing
  _map_widget->invalidateObjectLayer();
}

}  // namespace atlas
