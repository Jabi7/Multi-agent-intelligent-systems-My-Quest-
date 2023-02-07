#include <iostream>
#include <vector>
#include <random>

const int NUM_AGENTS = 2;
const int NUM_EPISODES = 1000;
const int NUM_STEPS = 100;
const float LEARNING_RATE = 0.1;
const float DISCOUNT_FACTOR = 0.9;
const float EPSILON = 0.1;

class Agent {
 public:
  Agent(int numStates, int numActions) {
    qTable_.resize(numStates, std::vector<float>(numActions, 0.0));
    policy_.resize(numStates, 0);
  }

  void UpdateQTable(int state, int action, int nextState, float reward) {
    qTable_[state][action] += LEARNING_RATE * (reward + DISCOUNT_FACTOR * MaxQValue(nextState) - qTable_[state][action]);
  }

  int ChooseAction(int state) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0, 1);

    if (dis(gen) < EPSILON) {
      // Choose a random action
      std::uniform_int_distribution<> actionDis(0, policy_[state].size() - 1);
      return actionDis(gen);
    } else {
      // Choose the action with the highest Q-value
      return MaxQIndex(state);
    }
  }

 private:
  float MaxQValue(int state) {
    float maxQ = qTable_[state][0];
    for (int i = 1; i < qTable_[state].size(); i++) {
      if (qTable_[state][i] > maxQ) {
        maxQ = qTable_[state][i];
      }
    }
    return maxQ;
  }

  int MaxQIndex(int state) {
    int maxIndex = 0;
    for (int i = 1; i < qTable_[state].size(); i++) {
      if (qTable_[state][i] > qTable_[state][maxIndex]) {
        maxIndex = i;
      }
    }
    return maxIndex;
  }

  std::vector<std::vector<float>> qTable_;
  std::vector<int> policy_;
};

int main() {
  std::vector<Agent> agents(NUM_AGENTS, Agent(100, 4));

  for (int episode = 0; episode < NUM_EPISODES; episode++) {
    for (int step = 0; step < NUM_STEPS; step++) {
      for (int i = 0; i < NUM_AGENTS; i++) {
        int state = 0; // Get current state
        int action = agents[i].ChooseAction(state);
        int nextState = 0; // Get next state based on state and action
        float reward = 0.0; // Get reward based on state and action
        agents[i].UpdateQTable(state, action, nextState, reward);
      }
    }
  }

  return 0;
}

